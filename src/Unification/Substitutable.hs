{-# LANGUAGE FlexibleInstances #-}

module Unification.Substitutable where

import           Data.Foldable (toList)
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Map as Map
import           Data.Map (Map)
import           Types.EType
import           Types.Expr (BareExpr)
import Unification.Scheme
import Control.Comonad.Cofree

class Substitutable a where
  apply :: Subst -> a -> a
  freeTyVars :: a -> Set TV

newtype Subst = Subst { unSubst :: Map TV EType }
  deriving Show

instance Substitutable EType where
  apply s@(Subst m) t =
    case t of
      TyVar tv  -> fromMaybe t $ Map.lookup tv m
      t1 :-> t2 -> apply s t1 :-> apply s t2
      _         -> t
  freeTyVars t =
    case t of
      TyVar tv  -> Set.singleton tv
      t1 :-> t2 -> freeTyVars t1 `Set.union` freeTyVars t2
      _         -> Set.empty

instance Substitutable Scheme where
  apply s (Forall tvs t) = Forall tvs $ apply (deletes s tvs) t
  freeTyVars (Forall tvs t) = freeTyVars t `Set.difference` Set.fromList tvs

instance Monoid Subst where
  mempty = Subst Map.empty
  mappend s1 s2 = Subst $ Map.map (apply s1) (unSubst s2) `Map.union` unSubst s1

instance Substitutable (Cofree BareExpr EType) where
  apply s (t :< e) = apply s t :< fmap (apply s) e
  freeTyVars (t :< e) = Set.unions (freeTyVars t : toList (fmap freeTyVars e))

delete :: TV -> Subst -> Subst
delete tv (Subst s) = Subst (Map.delete tv s)

deletes :: Subst -> [TV] -> Subst
deletes = foldr delete

singleton :: TV -> EType -> Subst
singleton tv t = Subst $ Map.singleton tv t
