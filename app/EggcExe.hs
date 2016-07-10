module Main where

import qualified Parser
import qualified Lexer
import qualified Compiler
import           LLVM (generateModule)
import           LLVM.General.Context (withContext)
import           Types.Expr (Expr)
import           Types.FunDef
import           System.IO (hPrint, stderr)
import           Control.Cofree

main :: IO ()
main = either print putStrLn =<< Compiler.compile =<< getContents
