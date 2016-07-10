module Test.Lex where

import qualified Lexer
import Types.Token (Lexeme(..), Token(..))
import Test.Tasty
import Test.Tasty.HUnit

lexerSpec :: TestTree
lexerSpec = testGroup "Lexer"
  [
    testCase "detecting identifiers" $
      testLexer "foo bar baz" [Identifier "foo", Identifier "bar", Identifier "baz"]
    ,

    testCase "detecting operators" $
      testLexer "foo -> bar" [Identifier "foo", Operator "->", Identifier "bar"]
    ,

    testCase "detecting keywords" $
      testLexer "func -> bar" [Keyword "func", Operator "->", Identifier "bar"]
  ]

testLexer :: String -> [Lexeme] -> Assertion
testLexer input expected = fmap (map _lexeme) (Lexer.lex "" input) @?= Right expected
