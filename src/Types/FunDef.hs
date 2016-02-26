module Types.FunDef where

import Types.Expr (Expr)

data FunDef = FunDef
  { _name :: String
  , _args :: [String]
  , _body :: Expr
  }
  deriving (Eq, Show)
