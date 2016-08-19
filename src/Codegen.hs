module Codegen (LlvmError, toAssembly) where

import           Codegen.Util
import           Control.Lens ((&))
import           Control.Monad (foldM, replicateM, void)
import           Control.Monad.Except (runExceptT)
import           Data.Bifunctor (first)
import           LLVM (generateModule, globalDefinition)
import           LLVM.General.AST (Definition)
import           LLVM.General.Context (withContext)
import           LLVM.General.Module (withModuleFromAST, moduleLLVMAssembly)
import           Types.Declaration
import           Unification
import           Unification.TyContext

newtype LlvmError = LlvmError String
  deriving (Show)

toAssembly :: [Declaration ()] -> IO (Either LlvmError String)
toAssembly defs = withContext $ \context -> let definitions = assembleDeclarations defs
                                                generatedModule = generateModule definitions "Egg!"
                                                errorOrAssembly = runExceptT $ withModuleFromAST context generatedModule moduleLLVMAssembly
                                            in first LlvmError <$> errorOrAssembly

assembleDeclarations :: [Declaration ()] -> [Definition]
assembleDeclarations ds = error "assembleDeclarations" $ typecheck ds

typecheck :: [Declaration ()] -> Either TypeError [Declaration Scheme]
typecheck ds = do
  let tyContext = initialContext ds
  -- HACK: to avoid type var collisions from the initial context we need to bump the supply. Not sure
  -- how to start with a different supply from `mempty`?
  fst <$> runInfer' tyContext (bumpSupply (length ds) >> mapM typeDeclaration ds)

bumpSupply :: Int -> Infer ()
bumpSupply n = void $ replicateM n freshVar

initialContext :: [Declaration a] -> TyContext
initialContext ds =
  let inferContext = foldM f mempty ds
  in runInfer' mempty inferContext & \case
       Right (inferred', []) -> inferred'
       _                     -> error "Error during initial declaration context generation!"
  where
    f env decl = do
      v <- freshVar
      pure (env +> (_name decl, Forall [] v))
