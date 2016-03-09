module Lexer (lex) where

import           Prelude hiding (lex)
import qualified Text.Megaparsec.Lexer as L
import           Control.Monad (void)
import           Text.Megaparsec
import           Types.Constant
import           Types.Token
import           Control.Applicative ((<$))

type Lexer = Parsec String

lex :: String -> String -> Either ParseError [Token]
lex = runParser (lexGrammar <* eof)

lexGrammar :: Lexer [Token]
lexGrammar = many . choice . map withPos $ [lexOperator, lexLiteral, try lexKeyword, lexIdentifier]

withPos :: Lexer Lexeme -> Lexer Token
withPos l = do
  lexeme <- l
  src <- getPosition
  pure $ Token lexeme src

spaceConsumer :: Lexer ()
spaceConsumer = L.space (void spaceChar) (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Lexer String
symbol = L.symbol spaceConsumer

lexOperator :: Lexer Lexeme
lexOperator = lexeme . choice . map (try . operator') $ operators
  where
    operator' :: String -> Lexer Lexeme
    operator' s = Operator <$> symbol s

operators = ["^", "->", "=", "+", "-", "(", "[", "{", "}", "]", ")", ",", ">", "<"]

lexKeyword = lexeme . choice . map keyword' $ reservedWords
  where
    keyword' :: String -> Lexer Lexeme
    keyword' w = Keyword <$> lexeme (string w)

reservedWords :: [String]
reservedWords = ["def", "if", "else", "while", "let", "true", "false"]

integer :: Lexer Integer
integer = lexeme L.integer <?> "integer"

lexLiteral :: Lexer Lexeme
lexLiteral = Literal . I <$> integer

lexIdentifier :: Lexer Lexeme
lexIdentifier = Identifier <$> lexeme baseIdentifier <?> "identifier"
  where
    baseIdentifier = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
