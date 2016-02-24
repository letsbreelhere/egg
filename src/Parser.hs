module Parser (parse) where

import           Text.Megaparsec.Prim hiding (token, parse)
import qualified Text.Megaparsec.Prim as Prim
import           Text.Megaparsec.Combinator
import           Text.Megaparsec.ShowToken (showToken)
import           Text.Megaparsec.Expr
import           Text.Megaparsec.Error (ParseError, Message(..))
import           Text.Megaparsec.Pos (updatePosChar)
import           Types.Token hiding (Literal)
import qualified Types.Token as Token
import           Types.Expr hiding (Literal)
import qualified Types.Expr as Expr
import           Types.Constant
import           Lexer
import Control.Applicative ((<$))

type Parser = Parsec [Token]

parse :: String -> [Token] -> Either ParseError Expr
parse = runParser program

program = expr <* eof

expr :: Parser Expr
expr = makeExprParser expr' table

table :: [[Operator Parser Expr]]
table = [ [mkInfix "+"]
        , [mkInfix ">"]
        ]
  where mkInfix name = InfixL (BinOp name <$ operator name)

expr' :: Parser Expr
expr' = choice [ Expr.Literal <$> literal <?> "literal"
               , function <?> "function definition"
               , assignment <?> "assignment"
               , ifExpr <?> "if statement"
               , try fnCall <?> "function call"
               , Var <$> anyIdentifier <?> "variable"
               , parens expr
               ]

assignment :: Parser Expr
assignment = do
  keyword "let"
  lhs <- anyIdentifier
  operator "="
  rhs <- expr
  return (Assign lhs rhs)

ifExpr :: Parser Expr
ifExpr = do
  keyword "if"
  predicate <- expr
  thenClause <- squareBraces expr
  keyword "else"
  elseClause <- squareBraces expr
  pure $ If predicate thenClause elseClause

literal :: Parser Constant
literal = choice [ I <$> anyInteger ]

parens = between (operator "(") (operator ")")
squareBraces = between (operator "[") (operator "]")
comma = operator ","

function :: Parser Expr
function = do
  keyword "def"
  name <- anyIdentifier
  args <- parens (anyIdentifier `sepBy` comma)
  body <- squareBraces expr
  return (Function name args body)

fnCall :: Parser Expr
fnCall = do
  name <- anyIdentifier
  args <- parens (expr `sepBy` comma)
  return (Call name args)

-- Primitives

withTycon :: (a -> Token) -> a -> Parser a
withTycon con a = a <$ token (con a)

keyword :: String -> Parser String
keyword = withTycon Keyword

operator :: String -> Parser String
operator = withTycon Operator

identifier :: String -> Parser String
identifier = withTycon Identifier

anyIdentifier :: Parser String
anyIdentifier = do
  Identifier i <- satisfy isIdentifier
  pure i

anyInteger :: Parser Integer
anyInteger = do
  Token.Literal l <- satisfy isLiteral
  case l of
    I i -> pure i
    _ -> failure [Unexpected (showToken l), Expected "integer"]

token :: Token -> Parser Token
token t = satisfy (== t) <?> showToken t

satisfy :: (Token -> Bool) -> Parser Token
satisfy p = Prim.token updatePosToken testToken
  where testToken t = if p t
                        then Right t
                        else Left . pure . Unexpected . showToken $ t
        updatePosToken _ pos _ = updatePosChar 0 pos ' '
