module TypeCheck where

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

data AnnState =
       AnnState
         { _tyVars :: Supply Int
         , _equations :: [Equation EType]
         , _symtab :: Map String EType
         }

defaultAnnState :: AnnState
defaultAnnState = AnnState naturals [] Map.empty

newtype TypeError = TypeError String

newtype Ann a = Ann { unAnn :: ExceptT TypeError (StateT AnnState Identity) a }

throwError :: String -> Ann a
throwError = Ann . throwE . TypeError

runAnn :: Ann a -> (Either TypeError a, AnnState)
runAnn = runIdentity . flip runStateT defaultAnnState . runExceptT . unAnn

constantType :: Constant -> EType
constantType c =
  case c of
    I _  -> Ty "int"
    C _  -> Ty "char"
    B _  -> Ty "bool"
    Unit -> Ty "void"

annotateDef :: Show a => FunDef a -> FunDef EType
annotateDef fd = fd { _body = annotate fd (_body fd) }

annotate :: Show b => FunDef a -> Expr' b -> AnnExpr
annotate cxt e@(e' :> _) =
  fmap (annotate cxt) e' :> resolveType cxt e

resolveType :: Show b => FunDef a -> Expr' b -> EType
resolveType cxt (e :> _) =
  case e of
    Literal c -> constantType c
    Var v -> fromMaybe
               (error $ "annotate: Variable " ++ show v ++ " does not exist in this context")
               (lookup v (_args cxt))
    If tyPred thn els ->
      let tyThen = resolveType cxt thn
          tyElse = resolveType cxt els
      in if tyThen == tyElse
           then tyThen
           else error "annotate: If/Else expressions don't match type"
    BinOp o _ _ -> Ty $ case o of
                     ">" -> "bool"
                     _   -> "int"
    _ -> error $ "annotate: unknown expression: " ++ show e
