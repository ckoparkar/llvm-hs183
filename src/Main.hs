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
-- import LLVM.Pretty (ppllvm)

import LLVM.OrcJIT  as LLVMJIT
import LLVM.Target  as LLVMJIT

import LLVM.Internal.ObjectFile
--------------------------------------------------------------------------------

defMain :: Definition
defMain = GlobalDefinition functionDefaults
  { name = Name "main_expr"
  , parameters = ([] , False)
  , returnType = i64
  , basicBlocks = [body]
  }
  where
    body = BasicBlock (Name "entry") [] (Do $ Ret Nothing [])


testmod :: LLVM.AST.Module
testmod = defaultModule
  { moduleName = "testmod"
  , moduleDefinitions = [defMain]
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
