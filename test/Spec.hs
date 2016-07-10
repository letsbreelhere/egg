module Main where

import Test.Lex
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests = testGroup "Suite"
  [
    lexerSpec
  ]
