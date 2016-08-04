module Unification.TyContext where

import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Monoid
import           Unification.Substitutable
import           Unification.Scheme

newtype TyContext = TyContext (Map String Scheme)

instance Monoid TyContext where
  mempty = TyContext mempty
  mappend (TyContext l) (TyContext r) = TyContext (l <> r)

instance Substitutable TyContext where
  apply s (TyContext env) = TyContext $ Map.map (apply s) env
  freeTyVars (TyContext env) = foldMap freeTyVars (Map.elems env)

infixl 0 +>

(+>) :: TyContext -> (String, Scheme) -> TyContext
TyContext cxt +> (v, scheme) = TyContext (Map.insert v scheme cxt)

lookupScheme :: String -> TyContext -> Maybe Scheme
lookupScheme v (TyContext c) = Map.lookup v c

addContext :: (String, Scheme) -> TyContext -> TyContext
addContext (x, t) (TyContext env) = TyContext $ Map.insert x t env

fromList :: [(String, Scheme)] -> TyContext
fromList = TyContext . Map.fromList
