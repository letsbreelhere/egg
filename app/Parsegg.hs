module Main where

import qualified Lexer
import qualified Parser

main :: IO ()
main = do
  input <- getContents
  let parsed = Parser.parse "" =<< Lexer.lex "" input
  either print (mapM_ print) parsed
