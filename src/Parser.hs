module Parser where

import           Text.Megaparsec (many, choice, (<|>), try, (<?>))
import           Text.Megaparsec.String (Parser)
import qualified Text.Megaparsec.Lexer as L
import           Types
import           Lexer

program = many stmt

stmt = choice [ while <?> "while statement"
              , assignment <* semicolon <?> "assignment"
              , E <$> expr <* semicolon
              ] <?> "statement"

expr :: Parser Expr
expr = choice [ Lit <$> literal <?> "literal"
              , Var <$> identifier <?> "variable"
              ] <?> "expression"

assignment :: Parser Statement
assignment = do
  lhs <- identifier
  symbol "="
  rhs <- expr
  return (lhs := rhs)

literal :: Parser Literal
literal = choice [ I <$> integer
                 , C <$> charLiteral
                 , S <$> stringLiteral
                 , Unit <$ symbol "()"
                 ]

while :: Parser Statement
while = do
  keyword "while"
  While <$> parens expr <*> curlyBraces program
