module Main where

import           Lib
import           Parser as Egg
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

codegen :: Expr -> IO String
codegen expr = withContext $ \context ->
  runOrBarf $ withModuleFromAST context newast moduleLLVMAssembly
  where
    definition = toDefinition expr
    newast = generateModule [definition] "Egg!"

main :: IO ()
main = do
  contents <- readFile "example.egg"
  let Right parsed = runParser Egg.program "example.egg" contents
  putStrLn =<< codegen parsed
