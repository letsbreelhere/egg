module TypeCheck where

import           Control.Cofree
import           Data.Maybe (fromMaybe)
import           Types.EType
import           Types.Expr
import           Types.FunDef
import           Types.Constant
import           Supply
import           Data.Map (Map)
import           Unification

data AnnState =
       AnnState
         { _tyVars :: Supply Int
         , _equations :: [Equation EType]
         , _symtab :: Map String EType
         }

constantType :: Constant -> EType
constantType c =
  case c of
    I _  -> Ty "int"
    C _  -> Ty "char"
    B _  -> Ty "bool"
    Unit -> Ty "void"

annotate :: FunDef -> Expr -> AnnExpr
annotate cxt e@(e' :> ()) =
  fmap (annotate cxt) e' :> resolveType cxt e

resolveType :: FunDef -> Expr -> EType
resolveType cxt (e :> ()) =
  case e of
    Literal c -> constantType c
    Var v -> fromMaybe (error $ "Variable " ++ show v ++ " does not exist in this context")
               (lookup v (_args cxt))
    If _ thn els -> let tyThen = resolveType cxt thn
                        tyElse = resolveType cxt els
                    in if tyThen == tyElse
                         then tyThen
                         else error "If/Else expressions don't match resolveTypepe"
    _ -> error $ "annotate: unknown expression: " ++ show e
