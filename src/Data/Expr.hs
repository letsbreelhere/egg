module Data.Expr where

import Control.Cofree
import Types.Expr
import Data.Set (Set)
import qualified Data.Set as Set

freeVariables :: String -> Expr -> Set String
freeVariables bound (expr :> _) = case expr of
  Var e | e /= bound -> Set.singleton e
  e1 :@: e2          -> foldMap (freeVariables bound) [e1,e2]
  BinOp _ e1 e2      -> foldMap (freeVariables bound) [e1,e2]
  If e1 e2 e3        -> foldMap (freeVariables bound) [e1,e2,e3]
  Lam bound' e       -> Set.delete bound $ freeVariables bound' e
  _                  -> Set.empty
