module Unification.TyContext where

import qualified Data.Map as Map
import           Data.Map (Map)
import           Unification.Substitutable
import           Unification.Scheme

newtype TyContext = TyContext (Map String Scheme)
  deriving (Monoid)

instance Substitutable TyContext where
  apply s (TyContext env) = TyContext $ Map.map (apply s) env
  freeTyVars (TyContext env) = foldMap freeTyVars (Map.elems env)

infixl 0 +>

(+>) :: TyContext -> (String, Scheme) -> TyContext
TyContext cxt +> (v, scheme) = TyContext (Map.insert v scheme cxt)

lookupCxt :: String -> TyContext -> Maybe Scheme
lookupCxt v (TyContext c) = Map.lookup v c

addContext :: (String, Scheme) -> TyContext -> TyContext
addContext (x, t) (TyContext env) = TyContext $ Map.insert x t env

fromList :: [(String, Scheme)] -> TyContext
fromList = TyContext . Map.fromList
