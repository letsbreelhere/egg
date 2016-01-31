module Main where

import           Lib
import           Parser as Egg
import qualified Data.Text.IO as T
import Text.Megaparsec (runParser)

main :: IO ()
main = do
  contents <- T.readFile "example.egg"
  print $ runParser Egg.program "example.egg" contents
