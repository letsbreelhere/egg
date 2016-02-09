module Types.Expr where

data Expr = Lit Literal
          | Var String
          | Call String [Expr]
  deriving (Eq, Show)

data Statement = E Expr
               | (:=) String Expr
               | While Expr [Statement]
  deriving Show

data Literal = I Integer
             | C Char
             | S String
             | Unit
  deriving (Eq, Show)
