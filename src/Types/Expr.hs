module Types.Expr (Expr(..), Literal(..)) where

data Expr = Lit Literal
          | Var String
          | Assign String Expr
          | Function String [String] Expr
          | Extern String [String]
          | Call String [Expr]
          | BinOp String Expr Expr
  deriving (Eq, Show)

data Literal = I Integer
             | C Char
             | S String
             | Unit
  deriving (Eq, Show)
