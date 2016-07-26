module Types.EType where

import Control.Monad (replicateM)

newtype TV = TV Int
  deriving (Eq, Ord)

instance Show TV where
  show (TV i) = names !! i
    where names = [1..] >>= flip replicateM ['a'..'z']

infixr 0 :->
data EType = Ty String
           | TyVar TV
           | EType :-> EType
  deriving (Eq)

instance Show EType where
  showsPrec _ (Ty t) = showString t
  showsPrec _ (TyVar tv@(TV _)) = shows tv
  showsPrec n (t1 :-> t2) = showParen (n > 0) $
                              showsPrec 1 t1 .
                              showString " -> " .
                              showsPrec n t2
