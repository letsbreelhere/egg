module Types where

import           Data.Text (Text)

data Literal = I Integer
             | C Char
             | T Text
             | Unit
  deriving (Eq, Show)

data Expr = Lit Literal
          | Var Text
          | Call Text [Expr]
  deriving (Eq, Show)

data Statement = E Expr
               | (:=) Text Expr
               | While Expr [Statement]
  deriving Show
