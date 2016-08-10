module Main where

import           Compiler
import           Control.Monad (forever)
import           Types.Expr
import           Data.Bifunctor (first)
import qualified Parser
import qualified Lexer
import           System.IO (hSetBuffering, stdout, BufferMode(..))
import Unification (typeOf)

respond :: Expr -> IO ()
respond expr = either print print (typeOf expr)

step :: IO ()
step = do
  putStr "> "
  input <- getLine
  let parsed = first ParseError $ Parser.parseExpr "" =<< Lexer.lex "" input
  either print respond parsed

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  forever step
