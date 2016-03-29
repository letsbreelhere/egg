module Main where

import qualified Lexer
import           Text.Megaparsec.ShowToken (showToken)
import           Data.List (intercalate)
import System.IO (hPrint, stderr)

main :: IO ()
main = do
  parsed <- Lexer.lex "" <$> getContents
  case parsed of
    Left err -> hPrint stderr err
    Right parsed' -> putStrLn . unwords . map showToken $ parsed'
