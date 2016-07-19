module Codegen (LlvmError, toAssembly) where

import qualified Data.Map as M
import           Control.Monad.Except (runExceptT)
import           Types.FunDef
import           LLVM (generateModule)
import           LLVM.General.Context (withContext)
import           LLVM.General.Module (withModuleFromAST, moduleLLVMAssembly)
import           Data.Bifunctor (first)
import           Codegen.Util
import           TypeCheck (globalCheckerEnv)

newtype LlvmError = LlvmError String
  deriving (Show)

toAssembly :: [FunDef ()] -> IO (Either LlvmError String)
toAssembly defs =
  let globalEnv = globalCheckerEnv defs
      definitions = concatMap (M.elems . functionToDefinitions globalEnv) defs
      generatedModule = generateModule definitions "Egg!"
  in withContext $ \context ->
    fmap (first LlvmError) . runExceptT $ withModuleFromAST context generatedModule
                                            moduleLLVMAssembly
