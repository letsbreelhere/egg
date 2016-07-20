module Types.EType where

data EType = Ty String
           | TyVar Int
           | EType :-> EType
  deriving (Eq)

instance Show EType where
  showsPrec _ (Ty t) = showString t
  showsPrec _ (TyVar k) = shows k
  showsPrec n (t1 :-> t2) = showParen (n > 0) $
                              showsPrec 1 t1 .
                              showString " -> " .
                              showsPrec n t2
