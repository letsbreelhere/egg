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
import           Data.Map (Map)
import qualified Data.Map as M

{- So here's the deal with this mess:
 - Currently, we require FunDefs to generate Definitions, which requires a
 - signature. Since I don't feel like extending the type system until I know
 - what closures even look like internally, I'm assuming they always take an
 - `int` and return an `int` for now.
 -
 - Also, `functionToDefinitions` has to be given as an argument here to avoid
 - cyclic imports, which is HORRIBLE. I think there's a hack to fix that
 - in the GHC docs somewhere.
 -}
generateLambda :: [(String, EType)] -> (FunDef EType -> Map String Definition) -> String -> AnnExpr -> Gen Operand
generateLambda freeVars functionToDefinitions v e = do
  closureName <- ("lam_" ++) <$> Gen.freshNamed
  let closureArgs = freeVars
      lambdaArg = (v, Ty "int")
      retTy = Ty "int"
      tmpDef = FunDef closureName (lambdaArg : closureArgs) e retTy
      llvmTy = ptr $ FunctionType i64 [i64] False
  closures %= mappend (functionToDefinitions tmpDef)
  closureVars %= M.insert closureName freeVars
  pure . ConstantOperand . LLVM.GlobalReference llvmTy . Name $ closureName
