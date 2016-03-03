module Control.Cofree where

import Data.Functor.Classes (Show1, showsPrec1)
import Data.Monoid

data Cofree f a = f (Cofree f a) :> a

type Fix f = Cofree f ()

instance Functor f => Functor (Cofree f) where
  fmap f (x :> a) = fmap (fmap f) x :> f a

instance (Show a, Show1 f) => Show (Cofree f a) where
  showsPrec n (x :> a) s = showsPrec1 n x "" ++ " :> " ++ showsPrec n a s

showLess :: (Functor f, Show a, Show1 f) => Cofree f a -> String
showLess (x :> _) = showsPrec1 1 (fmap showLess x) ""
