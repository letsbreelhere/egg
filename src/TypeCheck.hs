module TypeCheck (annotateDef) where

import           Control.Cofree
import           Data.Maybe (fromMaybe)
import           Types.EType
import           Types.Expr
import           Types.FunDef
import           Types.Constant
import           Supply
import           Data.Map (Map)
import qualified Data.Map as Map
import           Unification
import           Control.Monad.Trans.Except
import           Control.Monad.State
import           Control.Monad.Identity

constantType :: Constant -> EType
constantType c =
  case c of
    I _  -> Ty "int"
    C _  -> Ty "char"
    B _  -> Ty "bool"
    Unit -> Ty "void"

type Checker a = Either String a
type CheckerEnv = [(String, EType)]

annotateDef :: FunDef a -> Checker (FunDef EType)
annotateDef fd = (\newBody -> fd { _body = newBody }) <$> annotate fd (_body fd)

annotate :: FunDef a -> ExprTrans (Checker AnnExpr)
annotate cxt e@(e' :> _) = do
  typed <- resolveType (_args cxt) e
  annotated <- sequenceA $ fmap (annotate cxt) e'
  pure $ annotated :> typed

resolveType :: CheckerEnv -> ExprTrans (Checker EType)
resolveType env (e :> _) =
  case e of
    Literal c -> pure $ constantType c
    Var v -> maybe
               (Left $ "Variable " ++ show v ++ " does not exist in this context")
               Right
               (lookup v env)
    If tyPred thn els ->
      let tyThen = resolveType env thn
          tyElse = resolveType env els
      in if tyThen == tyElse
           then tyThen
           else error "annotate: If/Else expressions don't match type"
    BinOp o _ _ -> pure . Ty $ case o of
                     ">" -> "bool"
                     _   -> "int"
    -- TODO: when inference is implemented this should be able to infer argument
    -- type
    Lam v e -> let vTy = Ty "int"
                   env' = (v, vTy) : env
               in (vTy :->) <$> resolveType env' e
    _ -> Left "unknown expression"
