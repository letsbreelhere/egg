module Main where

import           Lib
import qualified Parser
import           Text.Megaparsec (runParser)
import           Control.Monad (forever)
import           System.IO

replRound :: IO ()
replRound = do
  putStr "egg> "
  hFlush stdout
  parsed <- runParser Parser.program "example.egg" <$> getLine
  either print print parsed

main :: IO ()
main = forever replRound
