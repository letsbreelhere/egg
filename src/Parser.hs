module Parser where

import           Text.Megaparsec (many, choice, (<|>), try, (<?>), sepBy)
import           Text.Megaparsec.String (Parser)
import qualified Text.Megaparsec.Lexer as L
import           Types
import           Lexer

program = expr

expr :: Parser Expr
expr = choice [ Lit <$> literal <?> "literal"
              , function <?> "function definition"
              , fnCall <?> "function call"
              , assignment <?> "assignment"
              , Var <$> identifier <?> "variable"
              ]

assignment :: Parser Expr
assignment = do
  keyword "let"
  lhs <- identifier
  symbol "="
  rhs <- expr
  return (Assign lhs rhs)

literal :: Parser Literal
literal = choice [ I <$> integer ]

function :: Parser Expr
function = do
  keyword "def"
  name <- identifier
  args <- parens (identifier `sepBy` comma)
  body <- squareBraces expr
  return (Function name args body)

fnCall :: Parser Expr
fnCall = do
  name <- identifier
  args <- parens (expr `sepBy` comma)
  return (Call name args)
