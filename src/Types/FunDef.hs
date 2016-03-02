module Types.FunDef where

import           Types.Expr (Expr)
import           Types.EType (EType)

type Signature = (String, EType)

data FunDef = FunDef { _name :: String, _args :: [Signature], _body :: Expr, _ret :: EType }
  deriving Show
