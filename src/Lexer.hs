{-# LANGUAGE FlexibleInstances #-}

module Lexer where

import           Prelude hiding (lex)
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.ShowToken
import           Control.Monad (void)
import           Text.Megaparsec
import           Types.Constant

data Token = Literal Constant
           | Identifier String
           | Keyword String
           | Operator String
  deriving Show

instance ShowToken Token where
  showToken t = case t of
    Literal c -> showToken c
    Identifier s -> s
    Keyword s -> s
    Operator s -> s

instance ShowToken [Token] where
  showToken = unwords . map showToken

type Lexer = Parsec String

lex :: String -> String -> Either ParseError [Token]
lex = runParser (lexGrammar <* eof)

lexGrammar :: Lexer [Token]
lexGrammar = many . choice $ [operator, keyword, literal, identifier]

spaceConsumer :: Lexer ()
spaceConsumer = L.space (void spaceChar) (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Lexer String
symbol = L.symbol spaceConsumer

operator :: Lexer Token
operator = lexeme . choice . map operator' $ ["=", "+", "-", "(", "[", "{", "}", "]", ")"]

operator' :: String -> Lexer Token
operator' s = Operator <$> symbol s

keyword = lexeme . choice . map keyword' $ reservedWords

reservedWords :: [String]
reservedWords = ["def"]

keyword' :: String -> Lexer Token
keyword' w = Keyword <$> lexeme (string w)

integer :: Lexer Integer
integer = lexeme L.integer <?> "integer"

literal :: Lexer Token
literal = Literal . I <$> integer

identifier :: Lexer Token
identifier = (check =<< lexeme baseIdentifier) <?> "identifier"
  where
    baseIdentifier = (:) <$> letterChar <*> many alphaNumChar
    check s = if s `elem` reservedWords
                then failWithMessage $ "Identifier `" ++ s ++ "' is reserved"
                else pure (Identifier s)

failWithMessage :: String -> Lexer a
failWithMessage msg = failure [Message msg]
