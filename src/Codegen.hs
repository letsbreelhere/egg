module Codegen where

import           Compiler
import           Control.Lens
import           Control.Monad.State (execState)
import           LLVM
import           LLVM.General.AST (Definition(..), Operand(..), BasicBlock)
import qualified LLVM.General.AST.Constant as Constant
import           LLVM.General.AST.Type (i64)
import           Types

toDefinition :: Statement -> Definition
toDefinition (E expr) = globalDefinition i64 "main" [] (mainBlocks expr)
toDefinition (v := e) = globalDefinition i64 "main" [] (assignmentBlocks v e)
toDefinition _ = undefined

mainBlocks :: Expr -> [BasicBlock]
mainBlocks expr = createBlocks . execCodegen $ ret =<< generateSimpleOperand expr

-- aka cgen
generateSimpleOperand :: Expr -> Gen Operand
generateSimpleOperand expr =
  case expr of
    Lit (I i) -> return $ ConstantOperand $ Constant.Int 64 (fromIntegral i)
    _         -> error $ "Not supported yet, doofus. Received: " ++ show expr ++ ""

assignmentBlocks :: String -> Expr -> [BasicBlock]
assignmentBlocks vname expr = createBlocks . execCodegen $ do
  var <- getVar vname
  value <- generateSimpleOperand expr
  store var value
  return ()
