module Parser (parse) where

import           Text.Megaparsec.Prim hiding (token, parse)
import qualified Text.Megaparsec.Prim as Prim
import           Text.Megaparsec.Combinator
import           Text.Megaparsec.ShowToken (showToken)
import           Text.Megaparsec.Expr
import           Text.Megaparsec.Error (ParseError, Message(..))
import           Types.Token
import qualified Types.Token as Token
import           Types.Expr
import qualified Types.Expr as Expr
import           Types.Constant
import           Types.Declaration
import           Control.Applicative (many)
import           Control.Monad (void)

type Parser = Parsec [Token]

parse :: String -> [Token] -> Either ParseError [Declaration ()]
parse = runParser program

program :: Parser [Declaration ()]
program = many function <* eof

function :: Parser (Declaration ())
function = do
  keyword "def"
  Declaration <$> anyIdentifier <*> squareBraces expr <*> pure Nothing

expr :: Parser Expr
expr = makeExprParser expr' table

table :: [[Operator Parser Expr]]
table = [[mkInfix "+", mkInfix "-", mkInfix "*"], [mkInfix ">"]]
  where
    mkInfix name = InfixL (binOp name <$ operator name)

expr' :: Parser Expr
expr' = choice
          [ Expr.literal <$> parseLiteral <?> "literal"
          , ifExpr <?> "if statement"
          , fnCall <?> "function call"
          , var <$> anyIdentifier <?> "variable"
          , parens expr
          , lambda
          ] <?> "expression"

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
  where
    bool = B <$> choice [True <$ keyword "true", False <$ keyword "false"]

fnCall :: Parser Expr
fnCall = do
  operator "`"
  call <$> expr <*> expr

lambda :: Parser Expr
lambda = do
  operator "^"
  v <- anyIdentifier
  operator "->"
  body <- expr
  pure (lam v body)

comma :: Parser ()
comma = void $ operator ","

parens :: Parser a -> Parser a
parens = between (operator "(") (operator ")")

squareBraces :: Parser a -> Parser a
squareBraces = between (operator "[") (operator "]")

withTycon :: (a -> Lexeme) -> a -> Parser ()
withTycon con a = void $ a <$ token (con a)

keyword :: String -> Parser ()
keyword = withTycon Keyword

operator :: String -> Parser ()
operator = withTycon Operator

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

token :: Lexeme -> Parser Lexeme
token t = satisfy (== t) <?> showToken t

satisfy :: (Lexeme -> Bool) -> Parser Lexeme
satisfy p = do
  t <- Prim.token updatePosToken testToken
  setPosition $ _pos t
  pure $ _lexeme t

  where
    testToken t = if p (_lexeme t)
                    then Right t
                    else Left . pure . Unexpected . showToken $ t
    updatePosToken _ pos _ = pos
