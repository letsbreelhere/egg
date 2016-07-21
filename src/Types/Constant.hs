module Types.Constant (Constant(..), showSimple) where

import           Text.Megaparsec.ShowToken

data Constant = I Integer
              | C Char
              | B Bool
              | Unit
  deriving (Ord, Eq, Show)

instance ShowToken Constant where
  showToken x = case x of
    I i  -> show i
    C c  -> show c
    B b  -> show b
    Unit -> "unit"

showSimple :: Constant -> String
showSimple = showToken
