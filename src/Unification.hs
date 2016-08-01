{-# LANGUAGE LambdaCase #-}

module Unification (
    runInfer,
    infer,
    Infer,
    freshVar,
    TyContext,
    (+>),
    TypeError(..),
    Scheme(..),
    runSolver,
    apply,
    finalApply,
    ) where

import           Control.Arrow ((***))
import           Control.Comonad.Cofree
import           Control.Monad.RWS
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import           Data.Set (Set)
import           Types.EType
import           Control.Monad.Except (MonadError, Except, runExcept, throwError)
import           Control.Monad.State (evalStateT, StateT)
import           Supply (Supply)
import qualified Supply
import           Types.Expr
import           Types.Constant

-- Classes
class Substitutable a where
  apply :: Subst -> a -> a
  freeTyVars :: a -> Set TV

-- Data types
newtype Subst = Subst { unSubst :: Map TV EType }
  deriving Show

newtype TyContext = TyContext (Map String Scheme)

newtype Infer a =
          Infer
            { unInfer :: RWST TyContext [(EType, EType)] (Supply TV) (Except TypeError) a }
  deriving (Functor, Applicative, Monad, MonadReader TyContext, MonadWriter [(EType, EType)], MonadState (Supply TV), MonadError TypeError)

data Scheme = Forall [TV] EType

data TypeError = InfiniteType TV EType
               | CantUnify EType EType
               | Unbound String
               | Unknown
  deriving (Eq)

-- Instances
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

closed :: Scheme -> Bool
closed (Forall tvs ty) =
  case ty of
    TyVar tv -> tv `elem` tvs
    l :-> r  -> closed (Forall tvs l) && closed (Forall tvs r)
    Ty _     -> True

instance Show Scheme where
  show (Forall tvs ty) = "forall " ++ unwords (map show tvs) ++ ". " ++ show ty

instance Eq Scheme where
  sch@(Forall _ ty) == sch'@(Forall _ ty') =
    let unified = runSolve $ unifies ty ty' >> solve
    in case unified of
      Left _  -> False
      Right s -> isIsomorphism s && closed sch && closed sch'
    where
      isIsomorphism :: Subst -> Bool
      isIsomorphism (Subst m) = all isTyVar $ Map.elems m

      isTyVar = \case
        TyVar _ -> True
        _       -> False

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

instance Show TypeError where
  show (InfiniteType tv ty) = "Can't construct the infinite type " ++ show tv ++ " ~ " ++ show ty
  show (CantUnify t u) = "Can't unify " ++ show t ++ " with " ++ show u
  show (Unbound v) = "Variable " ++ v ++ " is unbound"
  show Unknown = "Unknown expression encountered"

-- Functions
delete :: TV -> Subst -> Subst
delete tv (Subst s) = Subst (Map.delete tv s)

deletes :: Subst -> [TV] -> Subst
deletes = foldr delete

singleton :: TV -> EType -> Subst
singleton tv t = Subst $ Map.singleton tv t

type Unifier = (Subst, [(EType, EType)])

type Solve a = StateT Unifier (Except TypeError) a

runSolve :: Solve a -> Either TypeError a
runSolve m = runExcept (evalStateT m mempty)

runSolver :: [(EType, EType)] -> Either TypeError Subst
runSolver cs = runSolve $ solver (mempty, cs)

finalApply :: Subst -> Scheme -> Scheme
finalApply su (Forall _ t) =
  let t' = apply su t
  in Forall (Set.toList $ freeTyVars t') t'

unifies :: EType -> EType -> Solve Unifier
unifies t t'
  | t == t' = pure mempty
unifies (TyVar tv) t = bind tv t
unifies t (TyVar tv) = bind tv t
unifies (l :-> r) (l' :-> r') = unifyMany [(l, l'), (r, r')]
unifies t t' = throwError (CantUnify t t')

unifyMany :: [(EType, EType)] -> Solve Unifier
unifyMany [] = pure mempty
unifyMany ((t, t'):ts) = do
  (s, cs) <- unifies t t'
  (s', cs') <- unifyMany (map (apply s *** apply s) ts)
  pure (s' <> s, cs' <> cs)

solve :: Solve Subst
solve = do
  (s, cs) <- get
  case cs of
    [] -> pure s
    ((t, t'):cs1) -> do
      (s2, cs2) <- unifies t t'
      put (s2 <> s, cs2 <> map (apply s2 *** apply s2) cs1)
      solve

solver :: Unifier -> Solve Subst
solver su = put su *> solve

bind :: TV -> EType -> Solve Unifier
bind v t
  | occursCheck v t = throwError (InfiniteType v t)
  | otherwise = pure (singleton v t, [])

runInfer :: Infer EType -> Either TypeError (Scheme, [(EType, EType)])
runInfer (Infer m) =
  case runExcept (evalRWST m mempty tvSupply) of
    Left err -> Left err
    Right (ty, constraints) ->
      let scheme = Forall (Set.toList (freeTyVars ty)) ty
      in Right (scheme, constraints)
  where
    tvSupply = TV <$> Supply.naturals

occursCheck :: TV -> EType -> Bool
occursCheck tv ty = tv `Set.member` freeTyVars ty

unify :: EType -> EType -> Infer ()
unify t t' = tell [(t, t')]

freshVar :: Infer EType
freshVar = TyVar <$> state Supply.fresh

instantiate :: Scheme -> Infer EType
instantiate (Forall tvs ty) = do
  tvs' <- mapM (const freshVar) tvs
  let subst = Subst $ Map.fromList (tvs `zip` tvs')
  pure (apply subst ty)

lookupContext :: String -> TyContext -> Infer EType
lookupContext s env =
  case lookupScheme s env of
    Nothing     -> throwError (Unbound s)
    Just scheme -> instantiate scheme

inferConstant :: Constant -> EType
inferConstant (I _) = Ty "int"
inferConstant (C _) = Ty "char"
inferConstant (B _) = Ty "bool"
inferConstant Unit = Ty "unit"

infer :: ExprTrans (Infer EType)
infer (_ :< expr) =
  case expr of
    Var v -> lookupContext v =<< ask
    Literal c -> pure (inferConstant c)
    l :@: r -> do
      tv <- freshVar
      t <- infer l
      t' <- infer r
      unify t (t' :-> tv)
      pure tv
    If p thn els -> do
      t <- infer p
      t' <- infer thn
      t'' <- infer els
      unify t (Ty "bool")
      unify t' t''
      pure t'
    Lam v e -> do
      tv <- freshVar
      t <- inEnv (v, Forall [] tv) (infer e)
      pure (tv :-> t)
    _ -> throwError Unknown

inEnv :: (String, Scheme) -> Infer a -> Infer a
inEnv pair@(x, _) m = do
  let scope env = pair `addContext` remove env x
  local scope m

remove :: TyContext -> String -> TyContext
remove (TyContext m) v = TyContext $ Map.delete v m
