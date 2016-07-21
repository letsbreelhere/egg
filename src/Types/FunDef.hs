module Types.FunDef where

import           Types.Expr (Expr')
import           Types.EType (EType)
import Data.List (intercalate)

type Signature = (String, EType)

data FunDef ann = FunDef { _name :: String, _args :: [Signature], _body :: Expr' ann, _ret :: EType }
  deriving Eq

instance Show ann => Show (FunDef ann) where
  show f = _name f ++ "(" ++ showArgs (_args f) ++ ") : " ++ show (_ret f) ++ " = " ++ show (_body f)
    where showArgs = intercalate ", " . map (\(name, ty) -> name ++ " " ++ show ty)
