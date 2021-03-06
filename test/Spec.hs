module Main where

import Test.Tasty
import Test.Compiler
import Test.Lex
import Test.Expr
import Test.Parser
import Test.Unification

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Suite"
  [ lexerSpec
  , parserSpec
  , exprSpec
  , unificationSpec
  , compilerSpec
  ]
