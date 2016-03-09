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
import           LLVM.General.AST.Type (void, i64, i1)
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
toAssembly expr = withContext $ \context ->
  runOrBarf $ withModuleFromAST context generatedModule moduleLLVMAssembly
  where
    definitions = map functionToDefinition expr
    generatedModule = generateModule definitions "Egg!"
    runOrBarf :: ExceptT String IO a -> IO a
    runOrBarf = runExceptT >=> either fail return

functionToDefinition :: FunDef () -> Definition
functionToDefinition def =
  let def'  = annotateDef def
      args  = _args def'
      body  = _body def'
      retTy = _ret def'
      name  = _name def'
  in globalDefinition (reifyAbstractType retTy) name (map sigOf args) (bodyBlocks args body)

bodyBlocks :: [Signature] -> AnnExpr -> [BasicBlock]
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
    Ty "bool" -> i1
    Ty "int"  -> i64
    Ty "void" -> void

mainBlocks :: AnnExpr -> [BasicBlock]
mainBlocks expr = createBlocks . Gen.execCodegen $ ret =<< genOperand expr

genOperand :: AnnExpr -> Gen Operand
genOperand expr =
  case expr of
    Literal c   :> _  -> return . ConstantOperand . generateConstantOperand $ c
    Var v       :> _  -> load =<< getVar v
    l :@: r     :> _  -> generateApplication l r
    BinOp o l r :> _  -> generateOperator o l r
    If p t e    :> ty -> generateIf ty p t e

generateConstantOperand :: Constant -> LLVM.Constant
generateConstantOperand c =
  case c of
    I i -> LLVM.Int 64 $ fromIntegral i
    B b -> LLVM.Int 1 $ fromIntegral $ fromEnum b

generateApplication :: AnnExpr -> AnnExpr -> Gen Operand
generateApplication l r = error "Function application is borked, yo"

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

assignmentBlocks :: String -> AnnExpr -> [BasicBlock]
assignmentBlocks vname expr = createBlocks . Gen.execCodegen $ do
  var <- getVar vname
  value <- genOperand expr
  store var value
