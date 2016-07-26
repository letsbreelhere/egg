module Types.Declaration where

import           Types.Expr (Expr')
import           Types.EType (EType)

type Signature = (String, EType)

data Declaration ann = Declaration { _name :: String, _body :: Expr' ann }
  deriving Eq

instance Show ann => Show (Declaration ann) where
  show f = _name f ++ " = " ++ show (_body f)
