{-# LANGUAGE FlexibleInstances #-}

module Types.Token where

import           Text.Megaparsec.ShowToken
import           Types.Constant

data Token = Literal Constant
           | Identifier String
           | Keyword String
           | Operator String
  deriving (Show, Eq)

instance ShowToken Token where
  showToken t =
    case t of
      Literal c    -> showToken c
      Identifier s -> s
      Keyword s    -> s
      Operator s   -> s

instance ShowToken [Token] where
  showToken = unwords . map showToken

isIdentifier t = case t of
  Identifier _ -> True
  _ -> False

isOperator t = case t of
  Operator _ -> True
  _ -> False

isLiteral t = case t of
  Literal _ -> True
  _ -> False

isKeyword t = case t of
  Keyword _ -> True
  _ -> False
