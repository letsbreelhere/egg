module Main where

import qualified Lexer
import           Text.Megaparsec.ShowToken (showToken)
import           Data.List (intercalate)

main :: IO ()
main = do
  parsed <- Lexer.lex "" <$> getContents
  either print (putStrLn . intercalate ", " . map (show . showToken)) parsed
