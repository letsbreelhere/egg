{-# LANGUAGE LambdaCase #-}

module Unification (
    runInfer,
    runInfer',
    Infer,
    freshVar,
    TyContext,
    (+>),
    TypeError(..),
    Scheme(..),
    runSolver,
    apply,
    finalApply,
    infer,
    ) where

import           Control.Arrow ((***))
import           Control.Comonad.Cofree
import           Control.Monad.RWS
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Types.EType
import           Control.Monad.Except (MonadError, Except, runExcept, throwError)
import           Control.Monad.State (evalStateT, StateT)
import           Supply (Supply)
import qualified Supply
import           Types.Expr
import           Types.Constant
import           Unification.Scheme
import           Unification.Substitutable
import           Unification.TyContext

type Constraint = (EType, EType)

newtype Infer a = Infer (RWST TyContext [Constraint] (Supply TV) (Except TypeError) a)
  deriving (Functor, Applicative, Monad, MonadReader TyContext, MonadWriter [Constraint], MonadState (Supply TV), MonadError TypeError)

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

type Unifier = (Subst, [Constraint])

type Solve a = StateT Unifier (Except TypeError) a

runSolve :: Solve a -> Either TypeError a
runSolve m = runExcept (evalStateT m mempty)

runSolver :: [Constraint] -> Either TypeError Subst
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

unifyMany :: [Constraint] -> Solve Unifier
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

runInfer' :: TyContext -> Infer a -> Either TypeError (a, [Constraint])
runInfer' cxt (Infer m) = runExcept (evalRWST m cxt tvSupply)
  where
    tvSupply = TV <$> Supply.naturals

runInfer :: TyContext -> Infer AnnExpr -> Either TypeError (AnnExpr, [Constraint])
runInfer = runInfer'

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

infer :: ExprTrans (Infer AnnExpr)
infer (_ :< expr) =
  case expr of
    Var v -> fmap (\t -> t :< Var v) $ lookupContext v =<< ask
    Literal c -> pure $ inferConstant c :< Literal c
    l :@: r -> do
      tv <- freshVar
      l'@(t :< _) <- infer l
      r'@(t' :< _) <- infer r
      unify t (t' :-> tv)
      pure (tv :< l' :@: r')
    If p thn els -> do
      p'@(t :< _) <- infer p
      thn'@(t' :< _) <- infer thn
      els'@(t'' :< _) <- infer els
      unify t (Ty "bool")
      unify t' t''
      pure (t' :< If p' thn' els')
    Lam v e -> do
      tv <- freshVar
      e'@(t :< _) <- inEnv (v, Forall [] tv) (infer e)
      pure ((tv :-> t) :< Lam v e')
    _ -> throwError Unknown

inEnv :: (String, Scheme) -> Infer a -> Infer a
inEnv pair@(x, _) m = do
  let scope env = pair `addContext` remove env x
  local scope m

remove :: TyContext -> String -> TyContext
remove (TyContext m) v = TyContext $ Map.delete v m
