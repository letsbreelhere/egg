module Types.FunDef where

import           Control.Cofree
import           Types.Expr (Expr')
import           Types.EType (EType)

type Signature = (String, EType)

data FunDef ann = FunDef { _name :: String, _args :: [Signature], _body :: Expr' ann, _ret :: EType }
  deriving Eq

instance Show (FunDef ann) where
  show f = "FunDef " ++ _name f ++ " " ++ show (_args f) ++ " : " ++ show (_ret f) ++ " = " ++ showLess (_body f)
