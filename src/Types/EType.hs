module Types.EType where

data EType = Ty String
           | TVar Int
  deriving (Eq)
