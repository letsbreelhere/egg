module Parser where

import           Text.Megaparsec.Combinator
import           Text.Megaparsec.Expr
import           Text.Megaparsec.String (Parser)
import           Types
import           Lexer

program = expr <* eof

expr :: Parser Expr
expr = makeExprParser expr' table

table :: [[Operator Parser Expr]]
table = [ [mkInfix "+" (BinOp "+")]
        ]
  where mkInfix name f = InfixL (symbol name *> pure f)

expr' :: Parser Expr
expr' = choice [ Lit <$> literal <?> "literal"
              , function <?> "function definition"
              , assignment <?> "assignment"
              , try fnCall <?> "function call"
              , Var <$> identifier <?> "variable"
              , parens expr
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

expectedOps = ["+", "*"]
