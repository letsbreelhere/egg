module Main where

import           Lib
import qualified Lexer
import           Text.Megaparsec.ShowToken (showToken)
import           Text.Megaparsec (runParser)
import           Control.Monad (forever)
import           System.IO

replRound :: IO ()
replRound = do
  putStr "egg> "
  hFlush stdout
  parsed <- Lexer.lex "" <$> readFile "example.egg"
  either print (putStrLn . showToken) parsed

main :: IO ()
main = replRound
