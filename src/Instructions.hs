module Instructions where

import           Control.Lens hiding ((|>))
import           Control.Monad (void)
import           Data.Foldable (toList)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Sequence
import           LLVM.General.AST (Instruction, Name, Named(..), Operand(..), Terminator)
import qualified LLVM.General.AST as AST
import           LLVM.General.AST.IntegerPredicate (IntegerPredicate(..))
import           LLVM.General.AST.Type (i64, ptr)
import           Types.BlockState
import           Types.Gen
import           Types.GeneratorState
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.Constant as C

localReference :: Name -> Operand
localReference = AST.LocalReference i64

globalReference :: Name -> Operand
globalReference = ConstantOperand . C.GlobalReference i64

funRef :: Name -> Operand
funRef = ConstantOperand . C.GlobalReference funTy
  where
    funTy = ptr $ AST.FunctionType i64 [i64] False

call :: Operand -> Operand -> [Operand] -> Gen Operand
call fn lamarg closure = addInstruction $ AST.Call
                                            Nothing
                                            CC.C
                                            []
                                            (Right fn)
                                            (toArgs (lamarg : closure))
                                            []
                                            []
  where
    toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
    toArgs = map (\x -> (x, []))

addInstruction :: Instruction -> Gen Operand
addInstruction instr = do
  n <- freshUnnamed
  let ref = AST.UnName n
  currentBlockState . stack %= (|> (ref := instr))
  return (localReference ref)

add :: Operand -> Operand -> Gen Operand
add a b = addInstruction $ AST.Add False False a b []

mul :: Operand -> Operand -> Gen Operand
mul a b = addInstruction $ AST.Mul False False a b []

cmp :: IntegerPredicate -> Operand -> Operand -> Gen Operand
cmp c a b = addInstruction $ AST.ICmp c a b []

phi :: AST.Type -> [(Operand, Name)] -> Gen Operand
phi ty incoming = addInstruction $ AST.Phi ty incoming []

ne :: Operand -> Operand -> Gen Operand
ne = cmp NE

gt :: Operand -> Operand -> Gen Operand
gt = cmp SGT

sub :: Operand -> Operand -> Gen Operand
sub a b = addInstruction $ AST.Sub False False a b []

store :: Operand -> Operand -> Gen ()
store pointer val = void . addInstruction $ AST.Store False pointer val Nothing 0 []

alloca :: AST.Type -> Gen Operand
alloca t = addInstruction $ AST.Alloca t Nothing 0 []

load :: Operand -> Gen Operand
load pointer = addInstruction $ AST.Load False pointer Nothing 0 []

setTerminator :: Named Terminator -> Gen ()
setTerminator term = currentBlockState . terminator .= Just term

br :: Name -> Gen ()
br val = setTerminator $ AST.Do $ AST.Br val []

ret :: Operand -> Gen ()
ret val = setTerminator $ AST.Do $ AST.Ret (Just val) []

cbr :: Operand -> Name -> Name -> Gen ()
cbr cond tr fl = setTerminator $ AST.Do $ AST.CondBr cond tr fl []

getVar :: String -> Gen Operand
getVar vname = do
  value <- M.lookup vname <$> use symtab
  maybe (pure $ funRef (AST.Name vname)) load value

createBlocks :: GeneratorState -> ([AST.BasicBlock], M.Map String AST.Definition)
createBlocks gs = (toList $ M.mapWithKey makeBlock (view blocks gs), view closures gs)

makeBlock :: Name -> BlockState -> AST.BasicBlock
makeBlock l bs = AST.BasicBlock l (toList $ view stack bs) (castTerminator $ view terminator bs)
  where
    castTerminator = fromMaybe (error $ "Block has no terminator: " ++ show l)
