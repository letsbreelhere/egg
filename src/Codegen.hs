module Codegen (LlvmError, toAssembly) where

import           Control.Monad (foldM, replicateM, void)
import           Control.Monad.Except (runExceptT)
import           Types.Declaration
import           LLVM (generateModule, globalDefinition)
import           LLVM.General.Context (withContext)
import           LLVM.General.Module (withModuleFromAST, moduleLLVMAssembly)
import           Data.Bifunctor (first)
import           Codegen.Util
import           Unification
import           LLVM.General.AST (Definition)
import           Types.EType

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
  -- HACK: to avoid type var collisions from the initial context we need to bump
  -- the supply. Not sure how to start with a different supply from `mempty`?
  fst <$> runInfer' tyContext (bumpSupply (length ds) >> mapM typeDeclaration ds)

bumpSupply :: Int -> Infer ()
bumpSupply n = void $ replicateM n freshVar

initialContext :: [Declaration a] -> TyContext
initialContext ds =
  let inferContext = foldM f mempty ds
      Right (inferred, cs) = runInfer' mempty inferContext
  in inferred
 where
  f env decl = do
    v <- freshVar
    pure (env +> (_name decl, Forall [] v))
