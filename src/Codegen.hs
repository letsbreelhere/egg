module Codegen (LlvmError, toAssembly) where

import           Control.Monad (foldM)
import           Control.Monad.Except (runExceptT)
import           Types.Declaration
import           LLVM (generateModule, globalDefinition)
import           LLVM.General.Context (withContext)
import           LLVM.General.Module (withModuleFromAST, moduleLLVMAssembly)
import           Data.Bifunctor (first)
import           Codegen.Util
import           Unification
import           LLVM.General.AST (Definition)

newtype LlvmError = LlvmError String
  deriving (Show)

toAssembly :: [Declaration ()] -> IO (Either LlvmError String)
toAssembly defs = withContext $ \context ->
  let definitions = assembleDeclarations defs
      generatedModule = generateModule definitions "Egg!"
      errorOrAssembly = runExceptT $ withModuleFromAST context generatedModule moduleLLVMAssembly
  in first LlvmError <$> errorOrAssembly

assembleDeclarations :: [Declaration ()] -> [Definition]
assembleDeclarations = error "assembleDeclarations"

initialContext :: [Declaration a] -> Infer TyContext
initialContext = foldM f mempty
  where
    f env decl = do
      v <- freshVar
      pure (env +> (_name decl, Forall [] v))
