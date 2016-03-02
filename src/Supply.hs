{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Supply (Supply, fresh, variableNames, fromList) where

import           Control.Monad.State
import           Control.Applicative (ZipList(..))
import           Control.Arrow ((&&&))

newtype Supply a = Supply { unSupply :: ZipList a }
  deriving (Functor, Applicative)

instance Show a => Show (Supply a) where
  show s = "<supply: " ++ show (peek s) ++ ">"

peek :: Supply a -> a
peek = head . toList

fresh :: Supply a -> (a, Supply a)
fresh = peek &&& (fromList . tail . toList)

fromList :: [a] -> Supply a
fromList = Supply . ZipList

toList :: Supply a -> [a]
toList = getZipList . unSupply

variableNames :: Supply String
variableNames = fromList $ sequence =<< [replicate i ['a' .. 'z'] | i <- [1 ..]]

naturals :: Supply Int
naturals = fromList [0 ..]
