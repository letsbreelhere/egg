{-# LANGUAGE Rank2Types #-}

module Data.Expr where

import Control.Comonad.Cofree
import Types.Expr
import Data.Set (Set)
import qualified Data.Set as Set

freeVariables :: String -> ExprTrans (Set String)
freeVariables bound (_ :< expr) = case expr of
  Var e | e /= bound -> Set.singleton e
  e1 :@: e2          -> foldMap (freeVariables bound) [e1,e2]
  BinOp _ e1 e2      -> foldMap (freeVariables bound) [e1,e2]
  If e1 e2 e3        -> foldMap (freeVariables bound) [e1,e2,e3]
  Lam bound' e       -> Set.delete bound $ freeVariables bound' e
  _                  -> Set.empty
