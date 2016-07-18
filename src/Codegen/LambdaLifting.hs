module Codegen.LambdaLifting where

import           LLVM.General.AST (Name(..), Definition, Operand(..))
import           Types.Expr (AnnExpr)
import           Types.EType
import           Types.Gen (Gen)
import qualified Types.Gen as Gen
import           Types.FunDef
import           Control.Lens
import           Types.GeneratorState
import           LLVM.General.AST.Type (ptr, i64, Type(..))
import qualified LLVM.General.AST.Constant as LLVM

{- So here's the deal with this mess:
 - Currently, we require FunDefs to generate Definitions, which requires a
 - signature. Since I don't feel like extending the type system until I know
 - what closures even look like internally, I'm assuming they always take an
 - `int` and return an `int` for now.
 -}
generateLambda :: (FunDef EType -> [Definition]) -> String -> AnnExpr -> Gen Operand
generateLambda functionToDefinitions v e = do
  closureName <- ("lam_" ++) <$> Gen.freshNamed
  let closureArgs = [(v, Ty "int")]
      retTy = Ty "int"
      tmpDef = FunDef closureName closureArgs e retTy
      llvmTy = ptr $ FunctionType i64 [i64] False
  closures %= (++ functionToDefinitions tmpDef)
  return . ConstantOperand $ LLVM.GlobalReference llvmTy (Name closureName)
