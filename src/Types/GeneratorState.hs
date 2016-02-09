module Types.GeneratorState where

import           LLVM.General.AST (Instruction, Name(..), Named(..), Operand(..), Terminator)
import           Data.Map (Map)
import           Types.BlockState
import           Supply (Supply)
import qualified Supply
import           Control.Lens (Lens', lens, view, Identity, (&), (%~), _Just)
import qualified Data.Map as M
import qualified Data.Sequence as Seq

type SymbolTable = Map String Operand

data GeneratorState =
       GeneratorState
         { _activeBlock :: Name
         , _blocks :: Map Name BlockState
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

blocks :: Lens' GeneratorState (Map Name BlockState)
blocks = lens _blocks (\g s -> g { _blocks = s })

defaultGeneratorState = GeneratorState
  { _unnamedInstr = Supply.fromList [1 ..]
  , _namedInstr = Supply.variableNames
  , _blockCount = 1
  , _symtab = M.empty
  , _blocks = defaultBlockMap
  , _activeBlock = Name "entry"
  }

defaultBlockMap :: Map Name BlockState
defaultBlockMap = M.singleton (Name "entry") entryBlockState
  where entryBlockState = BlockState
          { _index = 0
          , _stack = Seq.empty
          , _terminator = Nothing
          }

currentBlockState :: (BlockState -> Identity BlockState) -> GeneratorState -> Identity GeneratorState
currentBlockState = lens getCurrentBlockState setCurrentBlockState . _Just
  where
    activeName = view activeBlock
    getCurrentBlockState gs = M.lookup (activeName gs) (view blocks gs)
    setCurrentBlockState gs Nothing = gs
    setCurrentBlockState gs (Just bs) = gs & blocks %~ M.insert (activeName gs) bs