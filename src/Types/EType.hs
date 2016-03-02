module Types.EType where

data EType = Ty String
           | TyVar Int
           | EType :-> EType
  deriving (Eq, Show)
