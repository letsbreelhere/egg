module Codegen (toAssembly) where

import           Control.Lens
import           Control.Monad (forM_, (>=>))
import           Control.Monad.Except (ExceptT, runExceptT)
import           Control.Monad.State (execState)
import qualified Data.Map as M
import           Instructions
import           LLVM (generateModule, globalDefinition)
import           LLVM.General.AST (Name(..), Definition, Operand(..), BasicBlock, Type)
import           LLVM.General.AST.Type (i64)
import qualified LLVM.General.AST.Constant as Constant
import           LLVM.General.Context (withContext)
import           LLVM.General.Module (withModuleFromAST, moduleLLVMAssembly)
import           Types.Constant (Constant(..))
import           Types.Expr (Expr(..))
import           Types.Gen (Gen)
import qualified Types.Gen as Gen
import           Types.GeneratorState
import           Types.BlockState (emptyBlock)

toAssembly :: Expr -> IO String
toAssembly expr = withContext $ \context ->
  runOrBarf $ withModuleFromAST context generatedModule moduleLLVMAssembly
  where
    definition = toDefinition expr
    generatedModule = generateModule [definition] "Egg!"
    runOrBarf :: ExceptT String IO a -> IO a
    runOrBarf = runExceptT >=> either fail return

toDefinition :: Expr -> Definition
toDefinition (Function name args body) = functionToDefinition name args body
toDefinition e = error $ "Unsupported top level expression: " ++ show e

functionToDefinition :: String -> [String] -> Expr -> Definition
functionToDefinition name args body = globalDefinition i64 name (map sigOf args) (bodyBlocks args body)

bodyBlocks args body = createBlocks . Gen.execCodegen $ do
  entryName <- Gen.addBlock "entry"
  activeBlock .= entryName
  forM_ args $ \arg -> do
    var <- alloca i64
    store var (localReference (Name arg))
    Gen.assign arg var
  ret =<< generateSimpleOperand body

sigOf :: String -> (Type, Name)
sigOf v = (i64, Name v)

mainBlocks :: Expr -> [BasicBlock]
mainBlocks expr = createBlocks . Gen.execCodegen $ ret =<< generateSimpleOperand expr

-- aka cgen
generateSimpleOperand :: Expr -> Gen Operand
generateSimpleOperand expr =
  case expr of
    Literal (I i) -> return $ ConstantOperand $ Constant.Int 64 (fromIntegral i)
    Var v         -> load =<< getVar v
    BinOp o l r   -> generateOperator o l r
    If p t e      -> generateIf p t e
    _             -> error $ "Not supported yet, doofus. Received: " ++ show expr

lift2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
lift2 f mx my = do
  x <- mx
  y <- my
  f x y

generateIf :: Expr -> Expr -> Expr -> Gen Operand
generateIf p t e = do
  ifThen <- Gen.addBlock "if.then"
  ifElse <- Gen.addBlock "if.else"
  ifExit <- Gen.addBlock "if.exit"
  predicate <- generateSimpleOperand p
  let false = ConstantOperand (Constant.Int 64 0)
  test <- ne false predicate
  cbr test ifThen ifElse

  activeBlock .= ifThen
  thenValue <- generateSimpleOperand t
  br ifExit
  ifThen <- use activeBlock

  activeBlock .= ifElse
  elseValue <- generateSimpleOperand e
  br ifExit
  ifElse <- use activeBlock

  activeBlock .= ifExit
  phi i64 [(thenValue, ifThen), (elseValue, ifElse)]

generateOperator :: String -> Expr -> Expr -> Gen Operand
generateOperator o l r = case o of
  "+" -> lift2 add (generateSimpleOperand l) (generateSimpleOperand r)
  ">" -> lift2 gt (generateSimpleOperand l) (generateSimpleOperand r)

assignmentBlocks :: String -> Expr -> [BasicBlock]
assignmentBlocks vname expr = createBlocks . Gen.execCodegen $ do
  var <- getVar vname
  value <- generateSimpleOperand expr
  store var value
