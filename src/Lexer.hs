module Lexer (lex) where

import           Prelude hiding (lex)
import qualified Text.Megaparsec.Lexer as L
import           Control.Monad (void)
import           Text.Megaparsec
import           Types.Constant
import           Types.Token
import Control.Applicative ((<$))

type Lexer = Parsec String

lex :: String -> String -> Either ParseError [Token]
lex = runParser (lexGrammar <* eof)

lexGrammar :: Lexer [Token]
lexGrammar = many . choice $ [lexOperator, lexKeyword, lexLiteral, lexIdentifier]

spaceConsumer :: Lexer ()
spaceConsumer = L.space (void spaceChar) (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Lexer String
symbol = L.symbol spaceConsumer

lexOperator :: Lexer Token
lexOperator = lexeme . choice . map operator' $ operators
  where
    operator' :: String -> Lexer Token
    operator' s = Operator <$> symbol s

operators = ["=", "+", "-", "(", "[", "{", "}", "]", ")", ","]

lexKeyword = lexeme . choice . map keyword' $ reservedWords
  where
    keyword' :: String -> Lexer Token
    keyword' w = Keyword <$> lexeme (string w)

reservedWords :: [String]
reservedWords = ["def", "if", "else", "while", "let"]

integer :: Lexer Integer
integer = lexeme L.integer <?> "integer"

lexLiteral :: Lexer Token
lexLiteral = Literal . I <$> integer

lexIdentifier :: Lexer Token
lexIdentifier = Identifier <$> lexeme baseIdentifier <?> "identifier"
  where
    baseIdentifier = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
