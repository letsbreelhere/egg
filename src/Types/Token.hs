{-# LANGUAGE FlexibleInstances #-}

module Types.Token where

import           Text.Megaparsec.ShowToken
import           Text.Megaparsec (SourcePos)
import           Types.Constant

data Lexeme = Literal Constant
            | Identifier String
            | Keyword String
            | Operator String
  deriving (Show, Eq)

data Token = Token { _lexeme :: Lexeme, _pos :: SourcePos }
  deriving (Show)

instance ShowToken Token where
  showToken t = showToken (_lexeme t)

instance ShowToken Lexeme where
  showToken l =
    case l of
      Literal c    -> "LT:" ++ showToken c
      Identifier s -> "ID:" ++ s
      Keyword s    -> "KW:" ++ s
      Operator s   -> "OP:" ++ s

instance ShowToken [Token] where
  showToken = unwords . map showToken

isIdentifier :: Lexeme -> Bool
isIdentifier t = case t of
  Identifier _ -> True
  _ -> False

isOperator :: Lexeme -> Bool
isOperator t = case t of
  Operator _ -> True
  _ -> False

isLiteral :: Lexeme -> Bool
isLiteral t = case t of
  Literal _ -> True
  _ -> False

isKeyword :: Lexeme -> Bool
isKeyword t = case t of
  Keyword _ -> True
  _ -> False
