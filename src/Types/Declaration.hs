module Types.Declaration where

import           Types.Expr (Expr)
import           Types.EType (EType)

type Signature = (String, EType)

data Declaration ann = Declaration
  { _name :: String
  , _body :: Expr
  , _ann  :: Maybe EType
  }
  deriving Eq

instance Show ann => Show (Declaration ann) where
  show f = concat [ _name f
                  , maybe "" ((++ " : ") . show) (_ann f)
                  , " = "
                  , show (_body f)
                  ]
