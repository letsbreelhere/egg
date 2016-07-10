module Test.Parser where

import qualified Lexer
import qualified Parser
import           Test.Tasty
import           Test.Tasty.HUnit
import           Types.Constant
import           Types.EType
import           Types.Expr
import           Types.FunDef

parserSpec = testGroup "Parser"
  [
    testCase "parsing a single function definition" $
      testParser
        (unlines [ "def const(i int) func (int) (int) ["
                 , "  ^ x -> 42"
                 , "]"
                 ]
        )
        [ FunDef
            { _name = "const"
            , _args = [("i", Ty "int")]
            , _body = lam "x" (literal (I 42))
            , _ret  = Ty "int" :-> Ty "int"
            }
        ]
  ]

testParser :: String -> [FunDef ()] -> Assertion
testParser input expected = (Parser.parse "" =<< Lexer.lex "" input) @?= Right expected
