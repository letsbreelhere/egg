module Main where

import qualified Parser
import qualified Lexer
import           Text.Megaparsec (runParser)
import           Codegen
import           Compiler
import           LLVM
import qualified LLVM.General.AST as AST
import           LLVM.General.Module
import           LLVM.General.Context
import           Types
import           Control.Monad.Except

runOrBarf :: ExceptT String IO a -> IO a
runOrBarf = runExceptT >=> either fail return

codegen :: Expr -> IO String
codegen expr = withContext $ \context ->
  runOrBarf $ withModuleFromAST context newast moduleLLVMAssembly
  where
    definition = toDefinition expr
    newast = generateModule [definition] "Egg!"

main :: IO ()
main = do
  contents <- getContents
  let parsed = Parser.parse "example.egg" =<< Lexer.lex "example.egg" contents
  case parsed of
    Left err -> print err
    Right parsed -> do
      putStrLn $ "; Parsed from " ++ show parsed
      putStrLn =<< codegen parsed
