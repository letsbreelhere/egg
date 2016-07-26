module TypeCheck (annotateDef, globalCheckerEnv, CheckerEnv) where

import           Control.Comonad.Cofree
import           Types.EType
import           Types.Expr
import           Types.Constant
import           Types.Declaration

constantType :: Constant -> EType
constantType c =
  case c of
    I _  -> Ty "int"
    C _  -> Ty "char"
    B _  -> Ty "bool"
    Unit -> Ty "void"

type Checker a = Either String a
type CheckerEnv = [(String, EType)]

globalCheckerEnv :: [Declaration a] -> CheckerEnv
globalCheckerEnv = map functionEntry

functionEntry :: Declaration a -> (String, EType)
functionEntry decl = (_name decl, undefined)

annotateDef :: CheckerEnv -> Declaration a -> Checker (Declaration EType)
annotateDef env decl = (\newBody -> decl { _body = newBody }) <$> annotate env decl (_body decl)

annotate :: CheckerEnv -> Declaration a -> ExprTrans (Checker AnnExpr)
annotate env cxt e@(_ :< e') = do
  typed <- resolveType env e
  annotated <- sequenceA $ fmap (annotate env cxt) e'
  pure $ typed :< annotated

resolveType :: CheckerEnv -> ExprTrans (Checker EType)
resolveType env (_ :< expr) =
  case expr of
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
