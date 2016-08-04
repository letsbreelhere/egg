module Unification.Scheme where

import           Types.EType

data Scheme = Forall [TV] EType

instance Show Scheme where
  show (Forall tvs ty) = "forall " ++ unwords (map show tvs) ++ ". " ++ show ty

closed :: Scheme -> Bool
closed (Forall tvs ty) =
  case ty of
    TyVar tv -> tv `elem` tvs
    l :-> r  -> closed (Forall tvs l) && closed (Forall tvs r)
    Ty _     -> True
