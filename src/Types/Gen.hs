{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies#-}
{-# LANGUAGE FlexibleInstances#-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types.Gen (
    Gen,
    addBlock,
    execCodegen,
    assign,
    lookup,
    freshNamed,
    freshUnnamed,
    ) where

import           Control.Lens ((%=), use, Identity)
import           Control.Monad.State (StateT, MonadState, execState, state)
import qualified Data.Map as M
import           LLVM.General.AST (Name(..), Operand(..))
import           Prelude hiding (lookup)
import           Types.GeneratorState
import           Types.BlockState (emptyBlock)
import           Supply (Supply)
import qualified Supply
import           Control.Lens.Zoom
import           Control.Lens.Internal.Zoom

newtype Gen' s a = Gen' { unGen :: StateT s Identity a }
  deriving (Functor, Applicative, Monad, MonadState s)

type Gen = Gen' GeneratorState

type instance Zoomed (Gen' s) = Focusing Identity

instance Zoom (Gen' s) (Gen' t) s t where
  zoom l (Gen' m) = Gen' (zoom l m)

execCodegen :: Gen a -> GeneratorState
execCodegen g = execState (unGen g) defaultGeneratorState

assign :: String -> Operand -> Gen ()
assign s o = symtab %= M.insert s o

lookup :: String -> Gen (Maybe Operand)
lookup var = M.lookup var <$> use symtab

fresh :: MonadState (Supply a) s => s a
fresh = state Supply.fresh

freshUnnamed :: Gen Word
freshUnnamed = zoom unnamedInstr fresh

freshNamed :: Gen String
freshNamed = zoom namedInstr fresh

addBlock :: String -> Gen Name
addBlock str = do
  let str' = str ++ "_"
  name <- Name . (str' ++) <$> freshNamed
  blocks %= M.insert name emptyBlock
  return name
