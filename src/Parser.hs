module Parser (parse) where

import           Text.Megaparsec.Prim hiding (token, parse)
import qualified Text.Megaparsec.Prim as Prim
import           Text.Megaparsec.Combinator
import           Text.Megaparsec.ShowToken
import           Text.Megaparsec.Expr
import           Text.Megaparsec.Error
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

identifier :: String -> Parser String
identifier i = i <$ token (Identifier i)

operator :: String -> Parser String
operator s = s <$ token (Operator s)

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

program = expr <* eof

expr :: Parser Expr
expr = makeExprParser expr' table

table :: [[Operator Parser Expr]]
table = [ [mkInfix "+"]
        ]
  where mkInfix name = InfixL (BinOp name <$ operator name)

expr' :: Parser Expr
expr' = choice [ Expr.Literal <$> literal <?> "literal" ]
              {-, function <?> "function definition"-}
              {-, assignment <?> "assignment"-}
              {-, try fnCall <?> "function call"-}
              {-, Var <$> identifier <?> "variable"-}
              {-, parens expr-}
              {-]-}

{-assignment :: Parser Expr-}
{-assignment = do-}
  {-keyword "let"-}
  {-lhs <- identifier-}
  {-symbol "="-}
  {-rhs <- expr-}
  {-return (Assign lhs rhs)-}

literal :: Parser Constant
literal = choice [ I <$> anyInteger ]

{-function :: Parser Expr-}
{-function = do-}
  {-keyword "def"-}
  {-name <- identifier-}
  {-args <- parens (identifier `sepBy` comma)-}
  {-body <- squareBraces expr-}
  {-return (Function name args body)-}

{-fnCall :: Parser Expr-}
{-fnCall = do-}
  {-name <- identifier-}
  {-args <- parens (expr `sepBy` comma)-}
  {-return (Call name args)-}

{-expectedOps = ["+", "*"]-}
