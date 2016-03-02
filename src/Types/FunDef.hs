module Types.FunDef where

import           Types.Expr (Expr')
import           Types.EType (EType)

type Signature = (String, EType)

data FunDef ann = FunDef { _name :: String, _args :: [Signature], _body :: Expr' ann, _ret :: EType }
  deriving Show
