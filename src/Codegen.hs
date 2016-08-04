module Codegen (LlvmError, toAssembly) where

import           Control.Monad.Except (runExceptT)
import           Types.Declaration
import           LLVM (generateModule)
import           LLVM.General.Context (withContext)
import           LLVM.General.Module (withModuleFromAST, moduleLLVMAssembly)
import           Data.Bifunctor (first)
import           Codegen.Util

newtype LlvmError = LlvmError String
  deriving (Show)

toAssembly :: [Declaration ()] -> IO (Either LlvmError String)
toAssembly defs =
  let definitions = assembleDeclarations defs
      generatedModule = generateModule definitions "Egg!"
  in withContext $ \context ->
    fmap (first LlvmError) . runExceptT $ withModuleFromAST context generatedModule
                                            moduleLLVMAssembly
