{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import           Control.Lens
import           Control.Monad.State
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import           Data.Text (Text)
import           LLVM.General.AST (Instruction, Name, Named(..), Operand(..), Terminator)
import           LLVM.General.AST.Type (i64)
import qualified LLVM.General.AST as AST
import           Supply (Supply)
import qualified Supply

data Literal = I Integer
             | C Char
             | T Text
             | Unit
  deriving (Eq, Show)

data Expr = Lit Literal
          | Var Text
          | Call Text [Expr]
  deriving (Eq, Show)

data Statement = E Expr
               | (:=) Text Expr
               | While Expr [Statement]
  deriving Show

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
  , _activeBlock = AST.Name "entry"
  }

defaultBlockMap :: Map Name BlockState
defaultBlockMap = M.singleton (AST.Name "entry") entryBlockState
  where entryBlockState = BlockState
          { _index = 0
          , _stack = Seq.empty
          , _terminator = Nothing
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

execCodegen :: Gen () -> GeneratorState
execCodegen g = execState (unGen g) defaultGeneratorState

localReference :: Name -> Operand
localReference = LocalReference i64

assign :: String -> Operand -> Gen ()
assign s o = symtab %= M.insert s o

lookup :: String -> Gen (Maybe Operand)
lookup var = M.lookup var <$> use symtab

currentBlockState :: (BlockState -> Identity BlockState) -> GeneratorState -> Identity GeneratorState
currentBlockState = lens getCurrentBlockState setCurrentBlockState . _Just
  where
    activeName = view activeBlock
    getCurrentBlockState gs = M.lookup (activeName gs) (view blocks gs)
    setCurrentBlockState gs Nothing = gs
    setCurrentBlockState gs (Just bs) = gs & blocks %~ M.insert (activeName gs) bs
