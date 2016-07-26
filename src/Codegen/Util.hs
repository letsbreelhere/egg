module Codegen.Util where

import Data.Foldable (toList)
import           Codegen.LambdaLifting
import           Control.Comonad.Cofree
import           Control.Lens hiding ((:<), op)
import           Control.Monad (forM_, foldM)
import           Data.Expr (freeVariables)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Types.Declaration
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
import           Unification

initialContext :: [Declaration a] -> Infer TyContext
initialContext = foldM (\env decl -> do { v <- freshVar; pure (env +> (_name decl, Forall [] v)) }) mempty

declarationToDefinitions :: Show a => CheckerEnv -> Declaration a -> Map String Definition
declarationToDefinitions env def =
  let def'@(t :< _) = either error id $ annotateDef env def
      body = _body def'
      name = _name def'
      (generatedBodyBlocks, cls) = bodyBlocks body
      mainFunctionDefn = globalDefinition (reifyAbstractType t) name []
                           generatedBodyBlocks
  in M.insert name mainFunctionDefn cls

bodyBlocks :: AnnExpr -> ([BasicBlock], Map String Definition)
bodyBlocks body = createBlocks . Gen.execCodegen $ do
  entryName <- Gen.addBlock "entry"
  activeBlock .= entryName
  ret =<< genOperand body

sigOf :: Signature -> (Type, Name)
sigOf (v, ty) = (reifyAbstractType ty, Name v)

reifyAbstractType :: EType -> Type
reifyAbstractType ty =
  case ty of
    t :-> u   -> ptr $ FunctionType (reifyAbstractType u) [reifyAbstractType t, i64] False
    Ty "bool" -> i1
    Ty "int"  -> i64
    Ty "void" -> void
    Ty t      -> error $ "Encountered unknown type " ++ show t
    TyVar _   -> error "Type variable encountered during reification"

genOperand :: AnnExpr -> Gen Operand
genOperand expr =
  case expr of
    _  :< Literal c   -> return . ConstantOperand . generateConstantOperand $ c
    _  :< Var v       -> getVar v
    _  :< l :@: r     -> generateApplication l r
    _  :< BinOp o l r -> generateOperator o l r
    ty :< If p t e    -> generateIf ty p t e
    _  :< Lam v e     -> let freeVars = toList $ freeVariables v e
                             sigs = map (\n -> (n, Ty "int")) freeVars
                         in generateLambda sigs (declarationToDefinitions []) v e

generateConstantOperand :: Constant -> LLVM.Constant
generateConstantOperand c =
  case c of
    I i -> LLVM.Int 64 $ fromIntegral i
    B b -> LLVM.Int 1 $ fromIntegral $ fromEnum b
    _   -> error "Encountered unknown constant"

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
  ifThen' <- use activeBlock

  activeBlock .= ifElse
  elseValue <- genOperand e
  br ifExit
  ifElse' <- use activeBlock

  activeBlock .= ifExit
  phi (reifyAbstractType ty) [(thenValue, ifThen'), (elseValue, ifElse')]

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
