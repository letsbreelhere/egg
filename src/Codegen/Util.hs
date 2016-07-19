module Codegen.Util where

import Data.Foldable (toList)
import           Codegen.LambdaLifting
import           Control.Lens
import           Control.Monad (forM_)
import           Data.Expr (freeVariables)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Types.FunDef
import           Types.EType
import           TypeCheck
import           Instructions
import           LLVM (globalDefinition)
import           LLVM.General.AST (Name(..), Definition, Operand(..), BasicBlock, Type)
import           LLVM.General.AST.Type (ptr, void, i64, i1, Type(..))
import qualified LLVM.General.AST.Constant as LLVM
import           Types.Constant (Constant(..))
import           Types.Expr (AnnExpr, BareExpr(..))
import           Types.Gen (Gen)
import qualified Types.Gen as Gen
import           Types.GeneratorState
import           Control.Cofree

functionToDefinitions :: Show a => CheckerEnv -> FunDef a -> Map String Definition
functionToDefinitions env def =
  let def' = either error id $ annotateDef env def
      args = _args def'
      body = _body def'
      retTy = _ret def'
      name = _name def'
      (generatedBodyBlocks, cls) = bodyBlocks args body
      mainFunctionDefn = globalDefinition (reifyAbstractType retTy) name (map sigOf args)
                           generatedBodyBlocks
  in M.insert name mainFunctionDefn cls

bodyBlocks :: [Signature] -> AnnExpr -> ([BasicBlock], Map String Definition)
bodyBlocks args body = createBlocks . Gen.execCodegen $ do
  entryName <- Gen.addBlock "entry"
  activeBlock .= entryName
  forM_ args $ \(arg, ty) -> do
    var <- alloca (reifyAbstractType ty)
    store var (localReference (Name arg))
    Gen.assign arg var
  ret =<< genOperand body

sigOf :: Signature -> (Type, Name)
sigOf (v, ty) = (reifyAbstractType ty, Name v)

reifyAbstractType :: EType -> Type
reifyAbstractType ty =
  case ty of
    t :-> u   -> ptr $ FunctionType (reifyAbstractType u) [reifyAbstractType t] False
    Ty "bool" -> i1
    Ty "int"  -> i64
    Ty "void" -> void

genOperand :: AnnExpr -> Gen Operand
genOperand expr =
  case expr of
    Literal c :> _   -> return . ConstantOperand . generateConstantOperand $ c
    Var v :> _       -> getVar v
    l :@: r :> _     -> generateApplication l r
    BinOp o l r :> _ -> generateOperator o l r
    If p t e :> ty   -> generateIf ty p t e
    Lam v e :> _     -> generateLambda (map (\n -> (n, error "Closure env typechecking")) . toList $ freeVariables v e) (functionToDefinitions []) v e

generateConstantOperand :: Constant -> LLVM.Constant
generateConstantOperand c =
  case c of
    I i -> LLVM.Int 64 $ fromIntegral i
    B b -> LLVM.Int 1 $ fromIntegral $ fromEnum b

generateApplication :: AnnExpr -> AnnExpr -> Gen Operand
generateApplication l r = do
  fn <- genOperand l
  lamarg <- genOperand r
  let fname = case fn of
                ConstantOperand (LLVM.GlobalReference _ (Name name)) -> Just name
                _ -> Nothing
  closureArgSigs <- case fname of
    Just name -> uses closureVars (M.lookup name)
    Nothing   -> pure Nothing
  closureEnv <- fetchClosureOperands (fromMaybe [] closureArgSigs)
  call fn lamarg closureEnv
  where fetchClosureOperands :: [(String, EType)] -> Gen [Operand]
        fetchClosureOperands = traverse (getVar . fst)

generateIf :: EType -> AnnExpr -> AnnExpr -> AnnExpr -> Gen Operand
generateIf ty p t e = do
  ifThen <- Gen.addBlock "if.then"
  ifElse <- Gen.addBlock "if.else"
  ifExit <- Gen.addBlock "if.exit"
  predicate <- genOperand p
  cbr predicate ifThen ifElse

  activeBlock .= ifThen
  thenValue <- genOperand t
  br ifExit
  ifThen <- use activeBlock

  activeBlock .= ifElse
  elseValue <- genOperand e
  br ifExit
  ifElse <- use activeBlock

  activeBlock .= ifExit
  phi (reifyAbstractType ty) [(thenValue, ifThen), (elseValue, ifElse)]

lift2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
lift2 f mx my = do
  x <- mx
  y <- my
  f x y

generateOperator :: String -> AnnExpr -> AnnExpr -> Gen Operand
generateOperator o l r =
  let op =
            case o of
              "+" -> add
              ">" -> gt
              "*" -> mul
              "-" -> sub
              _   -> error $ "Encountered unknown operator " ++ show o
  in lift2 op (genOperand l) (genOperand r)
