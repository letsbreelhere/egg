{-# LANGUAGE DeriveFunctor #-}

module Supply (Supply, fresh, variableNames, iterate, naturals) where

import           Prelude hiding (iterate)
import           Control.Monad.State
import           Control.Applicative (ZipList(..))
import           Control.Arrow ((&&&))
import           Data.Bifunctor (second)

data Supply a = a :& Supply a
  deriving (Functor)

instance Applicative Supply where
  pure a = a :& pure a
  (f :& fs) <*> (a :& as) = f a :& (fs <*> as)

peek :: Supply a -> a
peek (a :& _) = a

instance Show a => Show (Supply a) where
  show s = "<supply: " ++ show (peek s) ++ ">"

fresh :: Supply a -> (a, Supply a)
fresh (a :& as) = (a, as)

variableNames :: Supply String
variableNames = concatListSupply $ fmap (\n -> replicate (succ n) ['a' .. 'z']) naturals
  where
    concatListSupply :: Supply [a] -> Supply a
    concatListSupply ([] :& rest) = concatListSupply rest
    concatListSupply ((x:xs) :& rest) = x :& concatListSupply (xs :& rest)

iterate :: (a -> a) -> a -> Supply a
iterate f x = x :& iterate f (f x)

naturals :: Supply Int
naturals = iterate succ 0
