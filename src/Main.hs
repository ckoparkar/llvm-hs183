{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Data.Int
import Foreign.Ptr
import Data.ByteString.Char8 as BS
import LLVM.Module (Module, withModuleFromAST, moduleLLVMAssembly)
import LLVM.AST
import LLVM.AST.Global
import LLVM.Context
import LLVM.AST.Type
import LLVM.AST.Constant hiding (Add)
import LLVM.AST.CallingConvention
import LLVM.AST.AddrSpace
-- import LLVM.Pretty (ppllvm)

import LLVM.OrcJIT  as LLVMJIT
import LLVM.Target  as LLVMJIT

import LLVM.Internal.ObjectFile
--------------------------------------------------------------------------------

{-

`rts.c` has a minimal RTS I'm using to test the object file linking.
It houses the `main` function.

That main function calls `main_expr`, which is provided by this LLVM module.

Steps to reproduce the error:

(1) gcc -c -std=gnu11 rts.c

(2) cabal new-repl llvm-hs183

(3) > main


(The path for "rts.o" is not setup properly for `new-exec`. It works if an absolute path
is provided instead.)

-}


addTy :: Type
addTy = PointerType (FunctionType i64 [i64,i64] False) (AddrSpace 0)

defAdd :: Definition
defAdd = GlobalDefinition functionDefaults
  { name = Name "add"
  , parameters =
      ( [ Parameter i64 (Name "a") []
        , Parameter i64 (Name "b") [] ]
      , False )
  , returnType = i64
  , basicBlocks = [body]
  }
  where
    body = BasicBlock
        (Name "entry")
        [ Name "result" :=
            Add False  -- no signed wrap
                False  -- no unsigned wrap
                (LocalReference i64 (Name "a"))
                (LocalReference i64 (Name "b"))
                []]
        (Do $ Ret (Just (LocalReference i64 (Name "result"))) [])


defMain :: Definition
defMain = GlobalDefinition functionDefaults
  { name = Name "main_expr"
  , parameters = ([] , False)
  , returnType = i64
  , basicBlocks = [body]
  }
  where
    body = BasicBlock
        (Name "entry")
        [ Name "result" :=
            Call
              Nothing -- TailCallKind
              C       -- calling convention
              []      -- return attributes
              (Right (ConstantOperand $ GlobalReference addTy "add")) -- fn
              [ (ConstantOperand $ Int 64 40, [])
              , (ConstantOperand $ Int 64 2, [])] -- args
              [] -- function attributes
              [] -- metadata
        ]
        (Do $ Ret (Just (LocalReference i64 "result")) [])

testmod :: LLVM.AST.Module
testmod = defaultModule
  { moduleName = "testmod"
  , moduleDefinitions = [defAdd, defMain]
  }

--------------------------------------------------------------------------------

toLLVM :: LLVM.AST.Module -> IO ()
toLLVM mod = withContext $ \ctx -> do
  llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
  BS.putStrLn llvm

foreign import ccall "dynamic"
  mkMain :: FunPtr (IO Int32) -> IO Int32

withTestModule :: LLVM.AST.Module -> (LLVM.Module.Module -> IO a) -> IO a
withTestModule mod f = withContext $ \context -> withModuleFromAST context mod f

resolver :: IRCompileLayer l -> MangledSymbol -> IO JITSymbol
resolver compileLayer symbol
  = findSymbol compileLayer symbol True

nullResolver :: MangledSymbol -> IO JITSymbol
nullResolver _ = return (JITSymbol 0 (JITSymbolFlags False False))


eagerJIT :: LLVM.AST.Module -> IO Int32
eagerJIT amod =
    withTestModule amod $ \mod ->
      withHostTargetMachine $ \tm ->
        withObjectLinkingLayer $ \objectLayer ->
          withIRCompileLayer objectLayer tm $ \compileLayer -> do
            buf <- createObjectFile "rts.o"
            -- NOTE: This causes the memory corruption error
            _ <- addObjectFile objectLayer buf (SymbolResolver (resolver compileLayer) nullResolver)
            asm <- moduleLLVMAssembly mod
            BS.putStrLn asm
            -- return 42
            withModule
              compileLayer
              mod
              (SymbolResolver (resolver compileLayer) nullResolver) $
              \_ -> do
                mainSymbol <- mangleSymbol compileLayer "main_expr"
                JITSymbol mainFn _ <- findSymbol compileLayer mainSymbol True
                result <- mkMain (castPtrToFunPtr (wordPtrToPtr mainFn))
                return result

--------------------------------------------------------------------------------

main :: IO ()
main = do
    res <- eagerJIT testmod
    Prelude.putStrLn $ show res
    return ()
