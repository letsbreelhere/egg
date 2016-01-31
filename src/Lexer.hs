module Lexer where

import qualified Text.Megaparsec.Lexer as L
import           Control.Monad (void)
import           Text.Megaparsec
import           Text.Megaparsec.Text

spaceConsumer = L.space (void spaceChar) (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

curlyBraces = between (lexeme $ char '{') (lexeme $ char '}')

parens = between (lexeme $ char '(') (lexeme $ char ')')

squareBraces = between (lexeme $ char '[') (lexeme $ char ']')

keyword = lexeme . string

reservedWords = ["while"]

integer = lexeme L.integer

singleQuote :: Parser Char
singleQuote = char '\''
doubleQuote :: Parser Char
doubleQuote = char '"'

stringLiteral = lexeme (doubleQuote *> manyTill L.charLiteral doubleQuote)

charLiteral = lexeme (between singleQuote singleQuote L.charLiteral)

identifier = check =<< lexeme baseIdentifier
  where
    baseIdentifier = (:) <$> letterChar <*> many alphaNumChar
    check s = if s `elem` reservedWords
                then failure [Message $ "Identifier " ++ s ++ " is reserved"]
                else pure s

semicolon = symbol ";"
