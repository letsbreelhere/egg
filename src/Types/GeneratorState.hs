module Types.GeneratorState where

import           LLVM.General.AST (Definition, Name(..), Operand(..))
import           Types.BlockState
import           Supply (Supply)
import qualified Supply
import           Control.Lens (Traversal', Lens', lens, view, (&), (%~), _Just)
import           Data.Map (Map)
import qualified Data.Map as M
import Types.EType

type SymbolTable = Map String Operand

data GeneratorState =
       GeneratorState
         { _activeBlock :: Name
         , _blocks :: Map Name BlockState
         , _symtab :: SymbolTable
         , _unnamedInstr :: Supply Word
         , _namedInstr :: Supply String
         , _closures :: Map String Definition
         , _closureVars :: Map String [(String, EType)]
         }
  deriving Show

activeBlock :: Lens' GeneratorState Name
activeBlock = lens _activeBlock (\g s -> g { _activeBlock = s })

symtab :: Lens' GeneratorState SymbolTable
symtab = lens _symtab (\g s -> g { _symtab = s })

closures :: Lens' GeneratorState (Map String Definition)
closures = lens _closures (\g s -> g { _closures = s })

closureVars :: Lens' GeneratorState (Map String [(String, EType)])
closureVars = lens _closureVars (\g s -> g { _closureVars = s })

unnamedInstr :: Lens' GeneratorState (Supply Word)
unnamedInstr = lens _unnamedInstr (\g s -> g { _unnamedInstr = s })

namedInstr :: Lens' GeneratorState (Supply String)
namedInstr = lens _namedInstr (\g s -> g { _namedInstr = s })

blocks :: Lens' GeneratorState (Map Name BlockState)
blocks = lens _blocks (\g s -> g { _blocks = s })

defaultGeneratorState :: GeneratorState
defaultGeneratorState = GeneratorState
  { _unnamedInstr = fmap fromIntegral Supply.naturals
  , _namedInstr   = Supply.variableNames
  , _symtab       = M.empty
  , _blocks       = M.empty
  , _activeBlock  = Name "NOBLOCK"
  , _closures     = M.empty
  , _closureVars  = M.empty
  }

currentBlockState :: Traversal' GeneratorState BlockState
currentBlockState = lens getCurrentBlockState setCurrentBlockState . _Just
  where
    activeName = view activeBlock
    getCurrentBlockState gs = M.lookup (activeName gs) (view blocks gs)
    setCurrentBlockState gs Nothing = gs
    setCurrentBlockState gs (Just bs) = gs & blocks %~ M.insert (activeName gs) bs
