module TypeCheck (annotateDef, globalCheckerEnv, CheckerEnv) where

import           Control.Cofree
import           Types.EType
import           Types.Expr
import           Types.FunDef
import           Types.Constant

constantType :: Constant -> EType
constantType c =
  case c of
    I _  -> Ty "int"
    C _  -> Ty "char"
    B _  -> Ty "bool"
    Unit -> Ty "void"

type Checker a = Either String a
type CheckerEnv = [(String, EType)]

globalCheckerEnv :: [FunDef a] -> CheckerEnv
globalCheckerEnv = map functionEntry

functionEntry :: FunDef a -> (String, EType)
functionEntry fd = (_name fd, undefined :-> _ret fd)

annotateDef :: CheckerEnv -> FunDef a -> Checker (FunDef EType)
annotateDef env fd = (\newBody -> fd { _body = newBody }) <$> annotate env fd (_body fd)

annotate :: CheckerEnv -> FunDef a -> ExprTrans (Checker AnnExpr)
annotate env cxt e@(e' :> _) = do
  typed <- resolveType (env ++ _args cxt) e
  annotated <- sequenceA $ fmap (annotate env cxt) e'
  pure $ annotated :> typed

resolveType :: CheckerEnv -> ExprTrans (Checker EType)
resolveType env (e :> _) =
  case e of
    Literal c -> pure $ constantType c
    Var v -> maybe
               (Left $ "Variable " ++ show v ++ " does not exist in this context")
               Right
               (lookup v env)
    If _ thn els ->
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
    _ :@: _ -> pure $ Ty "int"
