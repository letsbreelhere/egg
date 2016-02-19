module Types.Constant (Constant(..)) where

import           Text.Megaparsec.ShowToken

data Constant = I Integer
              | C Char
              | S String
              | Unit
  deriving (Eq, Show)

instance ShowToken Constant where
  showToken c = case c of
    I i -> show i
    C c -> show c
    S s -> show s
