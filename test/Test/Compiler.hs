module Test.Compiler where

import qualified Parser
import qualified Lexer
import qualified Codegen
import Types.Token (Lexeme(..), Token(..))
import Test.Tasty
import Test.Tasty.HUnit

compilerSpec :: TestTree
compilerSpec = testGroup "Code generation and compiler"
  [ testCase "simple return value" $ testCompiler
      "def main() int [ 42 ]"
      "; ModuleID = 'Egg!'\n\ndefine i64 @main() {\nentry_a:\n  ret i64 42\n}\n"
  ]

testCompiler :: String -> String -> Assertion
testCompiler input expected = do
  let parsed = Parser.parse "" =<< Lexer.lex "" input
  case parsed of
    Left err -> assertFailure (show err)
    Right parsed' -> do assembly <- Codegen.toAssembly parsed'
                        assembly @?= expected
