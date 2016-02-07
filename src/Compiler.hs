{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compiler where

import qualified LLVM.General.AST as AST
import           LLVM.General.AST (Instruction, Name, Named(..), Operand(..), Terminator)
import           LLVM.General.AST.Type (i64)
import qualified Data.Map as M
import           Control.Lens hiding ((|>))
import           Control.Monad.State
import           Types (Expr)
import           Supply (Supply)
import qualified Supply
import           Data.Sequence

type SymbolTable = M.Map String Operand

data GeneratorState =
       GeneratorState
         { _activeBlock :: Name
         , _blocks :: M.Map Name BlockState
         , _symtab :: SymbolTable
         , _blockCount :: Int
         , _unnamedInstr :: Supply Word
         , _namedInstr :: Supply String
         }
  deriving Show

activeBlock :: Lens' GeneratorState Name
activeBlock = lens _activeBlock (\g s -> g { _activeBlock = s })

symtab :: Lens' GeneratorState SymbolTable
symtab = lens _symtab (\g s -> g { _symtab = s })

unnamedInstr :: Lens' GeneratorState (Supply Word)
unnamedInstr = lens _unnamedInstr (\g s -> g { _unnamedInstr = s })

namedInstr :: Lens' GeneratorState (Supply String)
namedInstr = lens _namedInstr (\g s -> g { _namedInstr = s })

blocks :: Lens' GeneratorState (M.Map Name BlockState)
blocks = lens _blocks (\g s -> g { _blocks = s })

defaultGeneratorState = GeneratorState
  { _unnamedInstr = Supply.fromList [1 ..]
  , _namedInstr = Supply.variableNames
  , _blockCount = 0
  , _symtab = M.empty
  , _blocks = M.empty
  , _activeBlock = AST.Name "entry"
  }

data BlockState =
       BlockState
         { _index :: Int
         , _stack :: Seq (Named Instruction)
         , _terminator :: Maybe (Named Terminator)
         }
  deriving Show

stack :: Lens' BlockState (Seq (Named Instruction))
stack = lens _stack (\g s -> g { _stack = s })

terminator :: Lens' BlockState (Maybe (Named Terminator))
terminator = lens _terminator (\g s -> g { _terminator = s })

newtype Gen a = Gen { unGen :: StateT GeneratorState Identity a }
  deriving (Functor, Applicative, Monad, MonadState GeneratorState)

localReference :: Name -> Operand
localReference = LocalReference i64

assign :: String -> Operand -> Gen ()
assign s o = symtab %= M.insert s o

lookup :: String -> Gen (Maybe Operand)
lookup var = M.lookup var <$> use symtab

currentBlockState = lens getCurrentBlockState setCurrentBlockState . _Just
  where
    activeName = view activeBlock
    getCurrentBlockState gs = M.lookup (activeName gs) (view blocks gs)
    setCurrentBlockState gs Nothing = gs
    setCurrentBlockState gs (Just bs) = gs & blocks %~ M.insert (activeName gs) bs

addInstruction :: Instruction -> Gen Operand
addInstruction instr = do
  (n, unnamedInstr') <- uses unnamedInstr Supply.fresh
  unnamedInstr .= unnamedInstr'
  let ref = AST.UnName n
  currentBlockState . stack %= (|> (ref := instr))
  return (localReference ref)

add :: Operand -> Operand -> Gen Operand
add a b = addInstruction $ AST.Add False False a b []

sub :: Operand -> Operand -> Gen Operand
sub a b = addInstruction $ AST.Sub False False a b []

br :: Name -> Gen ()
br val = currentBlockState . terminator .= Just (AST.Do $ AST.Br val [])

cbr :: Operand -> Name -> Name -> Gen ()
cbr cond tr fl = currentBlockState . terminator .= Just (AST.Do $ AST.CondBr cond tr fl [])

ret :: Operand -> Gen ()
ret val = currentBlockState . terminator .= Just (AST.Do $ AST.Ret (Just val) [])
