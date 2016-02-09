{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Gen (Gen, execCodegen, assign, lookup) where

import           Control.Lens ((%=), use, Identity)
import           Control.Monad.State (StateT, MonadState, execState)
import qualified Data.Map as M
import           LLVM.General.AST (Name, Operand(..))
import           LLVM.General.AST.Type (i64)
import           Prelude hiding (lookup)
import           Types.GeneratorState (GeneratorState, defaultGeneratorState, symtab)

newtype Gen a = Gen { unGen :: StateT GeneratorState Identity a }
  deriving (Functor, Applicative, Monad, MonadState GeneratorState)

execCodegen :: Gen () -> GeneratorState
execCodegen g = execState (unGen g) defaultGeneratorState

assign :: String -> Operand -> Gen ()
assign s o = symtab %= M.insert s o

lookup :: String -> Gen (Maybe Operand)
lookup var = M.lookup var <$> use symtab
