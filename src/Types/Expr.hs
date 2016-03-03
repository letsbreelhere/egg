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
    ) where

import           Data.Monoid
import           Types.EType
import           Types.Constant
import           Control.Cofree
import           Data.Functor.Classes (Show1, showsPrec1)

data BareExpr e = Literal Constant
                | Var String
                | Call String [e]
                | BinOp String e e
                | If e e e
  deriving (Eq, Show, Functor, Foldable)

instance Show1 BareExpr where
  showsPrec1 n e s = x ++ s
    where
      x =
        case e of
          Literal c    -> show c
          Var v        -> "%" ++ v
          Call n [as]  -> n ++ " " ++ show as
          BinOp s e e' -> show e ++ " " ++ s ++ " " ++ show e'
          If p t e     -> "if " ++ show p ++ " then " ++ show t ++ " else " ++ show e

instance Traversable BareExpr where
  sequenceA e = case e of
    Literal c    -> pure $ Literal c
    Var v        -> pure $ Var v
    Call nm args -> Call nm <$> sequenceA args
    BinOp o l r  -> BinOp o <$> l <*> r
    If p t e     -> If <$> p <*> t <*> e

type Expr' ann = Cofree BareExpr ann
type AnnExpr = Expr' EType
type Expr = Expr' ()

literal :: Constant -> Expr
literal c = Literal c :> ()

var :: String -> Expr
var s = Var s :> ()

call :: String -> [Expr] -> Expr
call s es = Call s es :> ()

binOp :: String -> Expr -> Expr -> Expr
binOp s l r = BinOp s l r :> ()

exprIf :: Expr -> Expr -> Expr -> Expr
exprIf p t e = If p t e :> ()
