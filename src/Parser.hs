module Parser where

import           Text.Megaparsec (many, choice, (<|>), try)
import           Text.Megaparsec.Text (Parser)
import qualified Data.Text as Txt
import           Data.Text (Text)
import qualified Text.Megaparsec.Lexer as L
import           Types
import           Lexer

program = many stmt

stmt = choice [ while
              , assignment <* semicolon
              , E <$> expr <* semicolon
              ]

expr :: Parser Expr
expr = (Lit <$> literal) <|> (Var <$> var)

assignment :: Parser Statement
assignment = do
  lhs <- var
  symbol "="
  rhs <- expr
  return (lhs := rhs)

literal :: Parser Literal
literal = I <$> integer
          <|> C <$> charLiteral
          <|> T . Txt.pack <$> stringLiteral
          <|> Unit <$ symbol "()"
  where

var :: Parser Text
var = Txt.pack <$> identifier

while :: Parser Statement
while = do
  keyword "while"
  While <$> parens expr <*> curlyBraces program
