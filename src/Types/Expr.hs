module Types.Expr (Expr(..)) where

import Types.Constant

data Expr = Literal Constant
          | Var String
          | Assign String Expr
          | Extern String [String]
          | Call String [Expr]
          | BinOp String Expr Expr
          | If Expr Expr Expr
  deriving (Eq, Show)
