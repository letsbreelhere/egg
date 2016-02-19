module Main where

import qualified Lexer
import qualified Parser
import           Data.List (intercalate)

main :: IO ()
main = do
  input <- getContents
  let parsed = Parser.parse "" =<< Lexer.lex "" input
  either print print parsed
