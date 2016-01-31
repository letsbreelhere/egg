module Main where

import           Lib
import qualified Parser
import qualified Data.Text.IO as T
import           Text.Megaparsec (runParser)
import           Control.Monad (forever)
import           System.IO

replRound :: IO ()
replRound = do
  putStr "egg> "
  hFlush stdout
  parsed <- runParser Parser.program "example.egg" <$> T.getLine
  either print print parsed

main :: IO ()
main = forever replRound
