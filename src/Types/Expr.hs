{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}

module Types.Expr (
    BareExpr(..),
    Expr',
    Expr,
    AnnExpr,
    literal,
    var,
    call,
    binOp,
    exprIf,
    lam,
    ) where

import           Data.Monoid
import           Types.EType
import           Types.Constant
import           Control.Cofree
import           Data.Functor.Classes (Eq1, eq1, Show1, showsPrec1)

infixl 9 :@:
data BareExpr e = Literal Constant
                | Var String
                | e :@: e
                | BinOp String e e
                | If e e e
                | Lam String e
  deriving (Eq, Show, Functor, Foldable)

instance Eq1 BareExpr where
  eq1 (Literal c) (Literal c') = c == c'
  eq1 (Var s) (Var s') = s == s'
  eq1 (a :@: b) (a' :@: b') = (a,b) == (a',b')
  eq1 (BinOp s e f) (BinOp s' e' f') = (s,e,f) == (s',e',f')
  eq1 (If p t e) (If p' t' e') = (p,t,e) == (p',t',e')
  eq1 (Lam v e) (Lam v' e') = (v,e) == (v',e')
  eq1 _ _ = False

instance Show1 BareExpr where
  showsPrec1 n e s = x ++ s
    where
      x =
        case e of
          Literal c    -> show c
          Var v        -> "%" ++ v
          l :@: r      -> show l ++ "@" ++ show r
          BinOp s e e' -> show e ++ " " ++ s ++ " " ++ show e'
          If p t e     -> "if " ++ show p ++ " then " ++ show t ++ " else " ++ show e
          Lam v e      -> "^ " ++ v ++ " -> " ++ show e

instance Traversable BareExpr where
  sequenceA e =
    case e of
      Literal c   -> pure $ Literal c
      Var v       -> pure $ Var v
      l :@: r     -> (:@:) <$> l <*> r
      BinOp o l r -> BinOp o <$> l <*> r
      If p t e    -> If <$> p <*> t <*> e

type Expr' ann = Cofree BareExpr ann

type AnnExpr = Expr' EType

type Expr = Expr' ()

literal :: Constant -> Expr
literal c = Literal c :> ()

var :: String -> Expr
var s = Var s :> ()

call :: Expr -> Expr -> Expr
call l r = l :@: r :> ()

binOp :: String -> Expr -> Expr -> Expr
binOp s l r = BinOp s l r :> ()

exprIf :: Expr -> Expr -> Expr -> Expr
exprIf p t e = If p t e :> ()

lam :: String -> Expr -> Expr
lam s e = Lam s e :> ()
