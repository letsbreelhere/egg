module Control.Cofree where

import Data.Functor.Classes (Show1, showsPrec1, Eq1, eq1)

data Cofree f a = f (Cofree f a) :> a

data Free f = Free { unroll :: f (Free f) }

instance (Show1 f) => Show (Free f) where
  showsPrec n f = showsPrec1 n (unroll f)

type Fix f = Cofree f ()

instance Functor f => Functor (Cofree f) where
  fmap f (x :> a) = fmap (fmap f) x :> f a

instance (Show a, Show1 f) => Show (Cofree f a) where
  showsPrec n (x :> _) = showsPrec1 n x

instance (Eq1 f, Eq a) => Eq (Cofree f a) where
  (l :> r) == (l' :> r') = l `eq1` l' && r == r'

stripAnnotation :: Functor f => Cofree f a -> Free f
stripAnnotation (cf :> _) = Free $ fmap stripAnnotation cf

showLess :: (Functor f, Show1 f, Show a) => Cofree f a -> String
showLess (x :> _) = showsPrec1 0 x ""
