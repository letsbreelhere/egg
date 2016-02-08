module Main where

import           Lib
import           Parser as Egg
import qualified Data.Text.IO as T
import           Text.Megaparsec (runParser)
import           Codegen
import           Compiler
import           LLVM
import qualified LLVM.General.AST as AST
import LLVM.General.Module
import LLVM.General.Context
import Types
import Control.Monad.Except

runOrBarf :: ExceptT String IO a -> IO a
runOrBarf = runExceptT >=> either fail return

codegen :: [Statement] -> IO String
codegen statements = withContext $ \context ->
  runOrBarf $ withModuleFromAST context newast moduleLLVMAssembly
  where
    definitions = map toDefinition statements
    newast = generateModule definitions "Egg!"

main :: IO ()
main = do
  contents <- T.readFile "example.egg"
  let Right parsed = runParser Egg.program "example.egg" contents
  putStrLn =<< codegen parsed
