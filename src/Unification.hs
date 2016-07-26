module Unification (runInfer, infer, TyContext, (+>), TypeError(..), Scheme(..)) where

import           Control.Comonad.Cofree
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Set as Set
import           Data.Set (Set)
import           Types.EType
import           Control.Monad.Except (MonadError, ExceptT, runExceptT, throwError)
import           Control.Monad.State (MonadState, State, evalState, state)
import           Supply (Supply)
import qualified Supply
import           Types.Expr
import           Types.Constant

newtype Subst = Subst { unSubst :: Map TV EType }
  deriving Show

isIsomorphism :: Subst -> Bool
isIsomorphism (Subst m) = all isTyVar $ Map.elems m
  where isTyVar ty = case ty of
                       TyVar _ -> True
                       _ -> False

delete :: TV -> Subst -> Subst
delete tv (Subst s) = Subst (Map.delete tv s)

deletes :: [TV] -> Subst -> Subst
deletes tvs s = foldr delete s tvs

singleton :: TV -> EType -> Subst
singleton tv t = Subst $ Map.singleton tv t

class Substitutable a where
  apply :: Subst -> a -> a
  freeTyVars :: a -> Set TV

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
  apply s (Forall tvs t) = Forall tvs $ apply (deletes tvs s) t
  freeTyVars (Forall tvs t) = freeTyVars t `Set.difference` Set.fromList tvs

instance Monoid Subst where
  mempty = Subst Map.empty
  mappend s1 s2 = Subst $ Map.map (apply s1) (unSubst s2) `Map.union` unSubst s1

data Scheme = Forall [TV] EType

closed :: Scheme -> Bool
closed (Forall tvs ty) = case ty of
  TyVar tv -> tv `elem` tvs
  l :-> r -> closed (Forall tvs l) && closed (Forall tvs r)
  Ty _ -> True

instance Show Scheme where
  show (Forall tvs ty) = "forall " ++ unwords (map show tvs) ++ ". " ++ show ty

instance Eq Scheme where
  sch@(Forall _ ty) == sch'@(Forall _ ty') =
    let unified = runUnify $ unify ty ty'
    in case unified of
         Left _ -> False
         Right s -> isIsomorphism s && closed sch && closed sch'

runUnify :: Infer Subst -> Either TypeError Subst
runUnify m = evalState (runExceptT (unInfer m)) (TV <$> Supply.naturals)

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

data TypeError = InfiniteType TV EType
               | CantUnify EType EType
               | Unbound String
               | Unknown
               deriving (Eq)

instance Show TypeError where
  show (InfiniteType tv ty) = "Can't construct the infinite type " ++ show tv ++ " ~ " ++ show ty
  show (CantUnify t u) = "Can't unify " ++ show t ++ " with " ++ show u
  show (Unbound v) = "Variable " ++ v ++ " is unbound"
  show Unknown = "Unknown expression encountered"

newtype Infer a = Infer { unInfer :: ExceptT TypeError (State (Supply TV)) a }
  deriving (Functor, Applicative, Monad, MonadState (Supply TV), MonadError TypeError)

runInfer :: Infer (Subst, EType) -> Either TypeError Scheme
runInfer m =
  case evalState (runExceptT (unInfer m)) tvSupply of
    Left err  -> Left err
    Right (s, ty) -> Right $ apply s (Forall (Set.toList (freeTyVars ty)) ty)
  where
    tvSupply = TV <$> Supply.naturals

occursCheck :: TV -> EType -> Bool
occursCheck tv ty = tv `Set.member` freeTyVars ty

unify :: EType -> EType -> Infer Subst
unify (l :-> r) (l' :-> r') = do
  s <- unify l l'
  s' <- unify (apply s r) (apply s r')
  pure (s <> s')
unify tyVar@(TyVar tv) ty
  | ty == tyVar = pure mempty
  | occursCheck tv ty = throwError (InfiniteType tv ty)
  | otherwise = pure (singleton tv ty)
unify ty tv@(TyVar _) = unify tv ty
unify t@(Ty _) u@(Ty _)
  | t == u = pure mempty
unify t u = throwError (CantUnify t u)

freshVar :: Infer EType
freshVar = TyVar <$> state Supply.fresh

instantiate :: Scheme -> Infer EType
instantiate (Forall tvs ty) = do
  tvs' <- mapM (const freshVar) tvs
  let subst = Subst $ Map.fromList (tvs `zip` tvs')
  pure (apply subst ty)

lookupContext :: String -> TyContext -> Infer (Subst, EType)
lookupContext s env =
  case lookupScheme s env of
    Nothing     -> throwError (Unbound s)
    Just scheme -> (,) mempty <$> instantiate scheme

inferConstant :: Constant -> EType
inferConstant (I _) = Ty "int"
inferConstant (C _) = Ty "char"
inferConstant (B _) = Ty "bool"
inferConstant Unit = Ty "unit"

infer :: TyContext -> ExprTrans (Infer (Subst, EType))
infer env (_ :< expr) =
  case expr of
    Var v -> lookupContext v env
    Literal c -> pure (mempty, inferConstant c)
    l :@: r -> do
      tv <- freshVar
      (s, t) <- infer env l
      (s', t') <- infer (apply s env) r
      s'' <- unify (apply s' t) (t' :-> tv)
      pure (s'' <> s' <> s, apply s'' tv)
    If p thn els -> do
      (s, t) <- infer env p
      (s', t') <- infer env thn
      (s'', t'') <- infer env els
      s''' <- unify t (Ty "bool")
      s'''' <- unify t' t''
      pure (mconcat [s'''', s''', s'', s', s], apply s'''' t')
    Lam v e -> do
      tv <- freshVar
      (s, t) <- infer (env +> (v, Forall [] tv)) e
      pure (s, apply s (tv :-> t))
    _ -> throwError Unknown
