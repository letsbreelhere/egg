{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, Rank2Types #-}

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
    ExprTrans,
    ) where

import           Types.EType
import           Types.Constant
import           Control.Comonad.Cofree
import           Data.Functor.Classes (Eq1, eq1, Show1, showsPrec1)

infixl 9 :@:

data BareExpr e = Literal Constant
                | Var String
                | (:@:) e e
                | BinOp String e e
                | If e e e
                | Lam String e
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Eq1 BareExpr where
  eq1 (Literal c) (Literal c') = c == c'
  eq1 (Var s) (Var s') = s == s'
  eq1 (a :@: b) (a' :@: b') = (a, b) == (a', b')
  eq1 (BinOp s e f) (BinOp s' e' f') = (s, e, f) == (s', e', f')
  eq1 (If p t e) (If p' t' e') = (p, t, e) == (p', t', e')
  eq1 (Lam v e) (Lam v' e') = (v, e) == (v', e')
  eq1 _ _ = False

cofreeTraverse :: (Applicative f, Traversable t) => (t (Cofree t a) -> f b) -> Cofree t a -> f (Cofree t b)
cofreeTraverse f (a :< m) = (:<) <$> f m <*> sequenceA (fmap (cofreeTraverse f) m)

instance Show1 BareExpr where
  showsPrec1 n expr =
    case expr of
      Literal c -> showString $ showSimple c
      Var v -> showString v
      l :@: r -> showParen (n > 10) $
        showString "`" .
        showsPrec n l .
        showString " " .
        showsPrec 10 r
      BinOp s e e' -> showParen (n > 1) $
        showsPrec 2 e .
        showString (" " ++ s ++ " ") .
        showsPrec 2 e'
      If p t e -> showString "if " .
                  showsPrec n p .
                  showString " then " .
                  showsPrec n t .
                  showString " else " .
                  showsPrec n e
      Lam v e -> showString "^ " . showString v . showString " -> " . showsPrec n e

type Expr' ann = Cofree BareExpr ann

type AnnExpr = Expr' EType

type Expr = Expr' ()

type ExprTrans t = forall ann. Cofree BareExpr ann -> t

literal :: Constant -> Expr
literal c = () :< Literal c

var :: String -> Expr
var s = () :< Var s

call :: Expr -> Expr -> Expr
call l r = () :< l :@: r

binOp :: String -> Expr -> Expr -> Expr
binOp s l r = () :< BinOp s l r

exprIf :: Expr -> Expr -> Expr -> Expr
exprIf p t e = () :< If p t e

lam :: String -> Expr -> Expr
lam s e = () :< Lam s e
