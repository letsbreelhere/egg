module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Lex
import Test.Parser

main :: IO ()
main = defaultMain tests

tests = testGroup "Suite"
  [ lexerSpec
  , parserSpec
  ]
