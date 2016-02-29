module Types.Constant (Constant(..)) where

import           Text.Megaparsec.ShowToken

data Constant = I Integer
              | C Char
              | B Bool
              | Unit
  deriving (Eq, Show)

instance ShowToken Constant where
  showToken c = case c of
    I i -> show i
    C c -> show c
    B b -> show b
