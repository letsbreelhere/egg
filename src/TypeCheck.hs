module TypeCheck (globalAnnotate) where

import           Control.Arrow (second)
import           Data.Monoid
import           Types.EType
import           Types.Declaration
import           Unification

globalAnnotate :: [Declaration a] -> Either TypeError [(Declaration a, Scheme)]
globalAnnotate decls = do
  ((ds, cs), cs') <- runInfer' mempty (globalAnnotate' decls)
  su <- runSolver (cs <> cs')
  pure $ map (second (finalApply su)) ds

globalAnnotate' :: [Declaration a] -> Infer ([(Declaration a, Scheme)], [(EType, EType)])
globalAnnotate' decls = do
  ts <- mapM (const freshVar) decls
  inferred <- mapM (infer . _body) decls
  let cs = ts `zip` inferred
  pure (decls `zip` map (Forall []) ts, cs)
