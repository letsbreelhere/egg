module Test.Parser where

import qualified Lexer
import qualified Parser
import           Test.Tasty
import           Test.Tasty.HUnit
import           Types.Constant
import           Types.Expr
import           Types.Declaration

parserSpec :: TestTree
parserSpec = testGroup "Parser"
  [
    testCase "parsing a single function definition" $
      testParser
        (unlines [ "def const ["
                 , "  ^ x -> 42"
                 , "]"
                 ]
        )
        [ Declaration
            { _name = "const"
            , _body = lam "x" (literal (I 42))
            , _ann = Nothing
            }
        ]
  ]

testParser :: String -> [Declaration ()] -> Assertion
testParser input expected = (Parser.parse "" =<< Lexer.lex "" input) @?= Right expected
