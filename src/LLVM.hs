module LLVM where

import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Control.Monad.Writer (MonadWriter, Writer, runWriter, tell)
import           LLVM.General.AST
import           LLVM.General.AST.Global
import           Data.Foldable (toList)

generateModule :: [Definition] -> String -> Module
generateModule defns label =
  defaultModule { moduleName = label, moduleDefinitions = defns }

globalDefinition :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> Definition
globalDefinition returnType label argumentTypes body = GlobalDefinition $ functionDefaults
  { name = Name label
  , parameters = ([Parameter ty nm [] | (ty, nm) <- argumentTypes], False)
  , returnType = returnType
  , basicBlocks = body
  }

externalDefinition :: Type -> String -> [(Type, Name)] -> Definition
externalDefinition returnType label argumentTypes = globalDefinition returnType label argumentTypes []
