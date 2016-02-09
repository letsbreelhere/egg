module Lexer where

import qualified Text.Megaparsec.Lexer as L
import           Control.Monad (void)
import           Text.Megaparsec
import           Text.Megaparsec.String

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

curlyBraces :: Parser a -> Parser a
curlyBraces = between (symbol "{") (symbol "}")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

squareBraces :: Parser a -> Parser a
squareBraces = between (symbol "[") (symbol "]")

keyword :: String -> Parser String
keyword w = lexeme (string w) <?> ("keyword " ++ show w)

reservedWords :: [String]
reservedWords = []

integer :: Parser Integer
integer = lexeme L.integer <?> "integer"

singleQuote :: Parser Char
singleQuote = char '\''

doubleQuote :: Parser Char
doubleQuote = char '"'

stringLiteral :: Parser String
stringLiteral = lexeme (doubleQuote *> manyTill L.charLiteral doubleQuote) <?> "string literal"

charLiteral :: Parser Char
charLiteral = lexeme (between singleQuote singleQuote L.charLiteral) <?> "character literal"

identifier :: Parser String
identifier = (check =<< lexeme baseIdentifier) <?> "identifier"
  where
    baseIdentifier = (:) <$> letterChar <*> many alphaNumChar
    check s = if s `elem` reservedWords
                then failWithMessage $ "Identifier `" ++ s ++ "' is reserved"
                else pure s

semicolon :: Parser String
semicolon = symbol ";"

failWithMessage :: String -> Parser a
failWithMessage msg = failure [Message msg]
