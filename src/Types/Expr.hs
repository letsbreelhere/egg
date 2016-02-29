module Types.Expr (
    BareExpr(..),
    Expr,
    AnnExpr,
    literal,
    var,
    extern,
    call,
    binOp,
    exprIf,
    ) where

import           Types.Constant
import           Control.Cofree
import Data.Functor.Classes (Show1, showsPrec1)

data BareExpr e = Literal Constant
                | Var String
                | Extern String [String]
                | Call String [e]
                | BinOp String e e
                | If e e e
  deriving (Eq)

instance Show1 BareExpr where
  showsPrec1 n e s = "<expr>" ++ s

type Expr = Fix BareExpr

type AnnExpr a = Cofree BareExpr a

literal :: Constant -> Expr
literal c = Literal c :> ()

var :: String -> Expr
var s = Var s :> ()

extern :: String -> [String] -> Expr
extern s ss = Extern s ss :> ()

call :: String -> [Expr] -> Expr
call s es = Call s es :> ()

binOp :: String -> Expr -> Expr -> Expr
binOp s l r = BinOp s l r :> ()

exprIf :: Expr -> Expr -> Expr -> Expr
exprIf p t e = If p t e :> ()
