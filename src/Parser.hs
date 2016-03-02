module Parser (parse) where

import           Text.Megaparsec.Prim hiding (token, parse)
import qualified Text.Megaparsec.Prim as Prim
import           Text.Megaparsec.Combinator
import           Text.Megaparsec.ShowToken (showToken)
import           Text.Megaparsec.Expr
import           Text.Megaparsec.Error (ParseError, Message(..))
import           Text.Megaparsec.Pos (updatePosChar)
import           Types.Token
import qualified Types.Token as Token
import           Types.EType
import           Types.Expr
import qualified Types.Expr as Expr
import           Types.Constant
import           Types.FunDef
import           Lexer
import           Control.Applicative (many, (<$))

type Parser = Parsec [Token]

parse :: String -> [Token] -> Either ParseError [FunDef ()]
parse = runParser program

program = many function <* eof

function :: Parser (FunDef ())
function = do
  keyword "def"
  name <- anyIdentifier
  args <- parens parseArgList
  ret  <- Ty <$> anyIdentifier
  body <- squareBraces expr
  return (FunDef name args body ret)

parseArgList :: Parser [Signature]
parseArgList = flip sepBy comma $ do
  arg <- anyIdentifier
  ty  <- anyIdentifier
  pure (arg, Ty ty)

expr :: Parser Expr
expr = makeExprParser expr' table

table :: [[Operator Parser Expr]]
table = [[mkInfix "+"], [mkInfix ">"]]
  where
    mkInfix name = InfixL (binOp name <$ operator name)

expr' :: Parser Expr
expr' = choice
          [ Expr.literal <$> parseLiteral <?> "literal"
          , ifExpr <?> "if statement"
          , try fnCall <?> "function call"
          , var <$> anyIdentifier <?> "variable"
          , parens expr
          ]

ifExpr :: Parser Expr
ifExpr = do
  keyword "if"
  predicate <- expr
  thenClause <- squareBraces expr
  keyword "else"
  elseClause <- squareBraces expr
  pure $ exprIf predicate thenClause elseClause

parseLiteral :: Parser Constant
parseLiteral = choice [I <$> anyInteger, bool]
  where bool = B <$> choice [True <$ keyword "true", False <$ keyword "false"]

parens = between (operator "(") (operator ")")

squareBraces = between (operator "[") (operator "]")

comma = operator ","

fnCall :: Parser Expr
fnCall = do
  name <- anyIdentifier
  args <- parens (expr `sepBy` comma)
  return (call name args)

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
    _   -> failure [Unexpected (showToken l), Expected "integer"]

token :: Token -> Parser Token
token t = satisfy (== t) <?> showToken t

satisfy :: (Token -> Bool) -> Parser Token
satisfy p = Prim.token updatePosToken testToken
  where
    testToken t = if p t
                    then Right t
                    else Left . pure . Unexpected . showToken $ t
    updatePosToken _ pos _ = updatePosChar 0 pos ' '
