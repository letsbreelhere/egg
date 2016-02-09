module Codegen where

import           Compiler
import           Control.Lens
import           Control.Monad.State (execState)
import           Control.Monad (forM_)
import qualified Data.Map as M
import           LLVM
import           LLVM.General.AST (Name(..), Definition(..), Operand(..), BasicBlock, Type)
import qualified LLVM.General.AST.Constant as Constant
import           LLVM.General.AST.Type (i64)
import           Types
import qualified Types.Gen as Gen

addBlock :: Gen Name
addBlock = do
  blockCount += 1
  ix <- use blockCount
  name <- Name <$> freshNamed
  blocks %= M.insert name (emptyBlock ix)
  return name

toDefinition :: Expr -> Definition
toDefinition (Function name args body) = globalDefinition i64 name (map sigOf args) bodyBlocks
  where bodyBlocks = createBlocks res
        res = execCodegen $ do
                       entryName <- addBlock
                       activeBlock .= entryName
                       forM_ args $ \arg -> do
                         var <- alloca i64
                         store var (localReference (Name arg))
                         Gen.assign arg var
                       ret =<< generateSimpleOperand body
toDefinition (Assign v e) = globalDefinition i64 "main" [] (assignmentBlocks v e)
toDefinition expr = globalDefinition i64 "main" [] (mainBlocks expr)

sigOf :: String -> (Type, Name)
sigOf v = (i64, Name v)

mainBlocks :: Expr -> [BasicBlock]
mainBlocks expr = createBlocks . execCodegen $ ret =<< generateSimpleOperand expr

-- aka cgen
generateSimpleOperand :: Expr -> Gen Operand
generateSimpleOperand expr =
  case expr of
    Lit (I i) -> return $ ConstantOperand $ Constant.Int 64 (fromIntegral i)
    _         -> error $ "Not supported yet, doofus. Received: " ++ show expr

assignmentBlocks :: String -> Expr -> [BasicBlock]
assignmentBlocks vname expr = createBlocks . execCodegen $ do
  var <- getVar vname
  value <- generateSimpleOperand expr
  store var value
  return ()
