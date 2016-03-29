module Codegen (toAssembly) where

import           Control.Lens
import           Control.Monad (forM_, (>=>))
import           Control.Monad.Except (ExceptT, runExceptT)
import           Control.Monad.State (execState)
import qualified Data.Map as M
import           Types.FunDef
import           Types.EType
import           TypeCheck
import           Instructions
import           LLVM (generateModule, globalDefinition)
import           LLVM.General.AST (Name(..), Definition, Operand(..), BasicBlock, Type)
import           LLVM.General.AST.AddrSpace (AddrSpace(..))
import           LLVM.General.AST.Type (ptr, void, i64, i1, Type(..))
import qualified LLVM.General.AST.Constant as LLVM
import           LLVM.General.Context (withContext)
import           LLVM.General.Module (withModuleFromAST, moduleLLVMAssembly)
import           Types.Constant (Constant(..))
import           Types.Expr (AnnExpr, BareExpr(..))
import           Types.Gen (Gen)
import qualified Types.Gen as Gen
import           Types.GeneratorState
import           Types.BlockState (emptyBlock)
import           Control.Cofree

toAssembly :: [FunDef ()] -> IO String
toAssembly defs = withContext $ \context ->
  runOrBarf $ withModuleFromAST context generatedModule moduleLLVMAssembly
  where
    definitions = concatMap functionToDefinitions defs
    generatedModule = generateModule definitions "Egg!"
    runOrBarf :: ExceptT String IO a -> IO a
    runOrBarf = runExceptT >=> either fail return

functionToDefinitions :: Show a => FunDef a -> [Definition]
functionToDefinitions def =
  let def'  = annotateDef def
      args  = _args def'
      body  = _body def'
      retTy = _ret def'
      name  = _name def'
      (generatedBodyBlocks, cls) = bodyBlocks args body
      mainFunctionDefn = globalDefinition (reifyAbstractType retTy) name (map sigOf args) generatedBodyBlocks
  in mainFunctionDefn : cls

bodyBlocks :: [Signature] -> AnnExpr -> ([BasicBlock], [Definition])
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
    Literal c   :> _  -> return . ConstantOperand . generateConstantOperand $ c
    Var v       :> _  -> load =<< getVar v
    l :@: r     :> _  -> generateApplication l r
    BinOp o l r :> _  -> generateOperator o l r
    If p t e    :> ty -> generateIf ty p t e
    Lam v e     :> _  -> generateLambda v e

{- So here's the deal with this mess:
 - Currently, we require FunDefs to generate Definitions, which requires a
 - signature. Since I don't feel like extending the type system until I know
 - what closures even look like internally, I'm assuming they always take an
 - `int` and return an `int` for now.
 -}
generateLambda :: String -> AnnExpr -> Gen Operand
generateLambda v e = do
  closureName <- ("lam_" ++) <$> Gen.freshNamed
  let closureArgs = [(v, Ty "int")]
      retTy = Ty "int"
      tmpDef = FunDef closureName closureArgs e retTy
      llvmTy = ptr $ FunctionType i64 [i64] False
  closures %= (++ functionToDefinitions tmpDef)
  return . ConstantOperand $ LLVM.GlobalReference llvmTy (Name closureName)

generateConstantOperand :: Constant -> LLVM.Constant
generateConstantOperand c =
  case c of
    I i -> LLVM.Int 64 $ fromIntegral i
    B b -> LLVM.Int 1 $ fromIntegral $ fromEnum b

generateApplication :: AnnExpr -> AnnExpr -> Gen Operand
generateApplication l r = lift2 callLambda (genOperand l) (genOperand r)

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
  case o of
    "+" -> lift2 add (genOperand l) (genOperand r)
    ">" -> lift2 gt (genOperand l) (genOperand r)
