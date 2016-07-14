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
  ret <- typeSignature
  body <- squareBraces expr
  return (FunDef name args body ret)

typeSignature :: Parser EType
typeSignature = choice [funTy, Ty <$> anyIdentifier]
  where funTy = do
          keyword "func"
          l <- parens typeSignature
          r <- parens typeSignature
          pure (l :-> r)

parseArgList :: Parser [Signature]
parseArgList = flip sepBy comma $ do
  arg <- anyIdentifier
  ty <- typeSignature
  pure (arg, ty)

expr :: Parser Expr
expr = makeExprParser expr' table

table :: [[Operator Parser Expr]]
table = [[mkInfix "+", mkInfix "-", mkInfix "*"], [mkInfix ">"]]
  where
    mkInfix name = InfixL (binOp <$> operator name)

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

comma = operator ","

parens = between (operator "(") (operator ")")

squareBraces = between (operator "[") (operator "]")

withTycon :: (a -> Lexeme) -> a -> Parser a
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
