module Main where

import           Lib
import qualified Lexer
import           Text.Megaparsec.ShowToken (showToken)
import           Text.Megaparsec (runParser)
import           Control.Monad (forever)
import           System.IO
import           Data.List (intercalate)

main :: IO ()
main = do
  parsed <- Lexer.lex "" <$> getContents
  either print (putStrLn . intercalate ", " . map (show . showToken)) parsed
