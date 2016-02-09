module Types.BlockState where

import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import           Control.Lens
import           LLVM.General.AST (Instruction, Name, Named(..), Operand(..), Terminator)

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

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState { _index = i, _stack = Seq.empty, _terminator = Nothing }
