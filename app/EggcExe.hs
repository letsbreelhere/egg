module Main where

import qualified Parser
import qualified Lexer
import qualified Codegen
import           LLVM (generateModule)
import           LLVM.General.Context (withContext)
import           Types.Expr (Expr)
import           Types.FunDef
import           System.IO (hPrint, stderr)
import           Control.Cofree

main :: IO ()
main = do
  contents <- getContents
  let parsed = Parser.parse "" =<< Lexer.lex "" contents
  case parsed of
    Left err -> hPrint stderr err
    Right parsed' -> do
      putStrLn $ "; Parsed from " ++ show (map (showLess . _body) parsed')
      putStrLn =<< Codegen.toAssembly parsed'
