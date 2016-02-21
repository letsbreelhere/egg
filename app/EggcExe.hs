module Main where

import qualified Parser
import qualified Lexer
import qualified Codegen
import           LLVM (generateModule)
import           LLVM.General.Context (withContext)
import           Types.Expr (Expr)
import System.IO (hPrint, stderr)

main :: IO ()
main = do
  contents <- getContents
  let parsed = Parser.parse "" =<< Lexer.lex "" contents
  case parsed of
    Left err -> hPrint stderr err
    Right parsed' -> do
      putStrLn $ "; Parsed from " ++ show parsed'
      putStrLn =<< Codegen.toAssembly parsed'
