module LLVM where

import           LLVM.General.AST
import           LLVM.General.AST.Global (Global(..))

generateModule :: [Definition] -> String -> Module
generateModule defns label =
  defaultModule { moduleName = label, moduleDefinitions = defns }

globalDefinition :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> Definition
globalDefinition retTy label argTys body = GlobalDefinition $ functionDefaults
  { name = Name label
  , parameters = ([Parameter ty nm [] | (ty, nm) <- argTys], False)
  , returnType = retTy
  , basicBlocks = body
  }

externalDefinition :: Type -> String -> [(Type, Name)] -> Definition
externalDefinition retTy label argTys = globalDefinition retTy label argTys []
