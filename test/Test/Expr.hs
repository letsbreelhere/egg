module Test.Expr where

import Test.Tasty
import Test.Tasty.HUnit
import Types.Expr
import Data.Expr
import qualified Data.Set as Set

exprSpec :: TestTree
exprSpec = testGroup "Syntax tree helpers"
  [ freeVariablesTest
  ]

freeVariablesTest :: TestTree
freeVariablesTest = testGroup "freeVariables"
  [ testCase "bare variables" $
      freeVariables "x" (var "x") @?=
      Set.empty

  , testCase "bare variables - no match" $
      freeVariables "x" (var "y") @?=
      Set.singleton "y"

  , testCase "sub-lambdas" $
      freeVariables "x" (lam "y" $ var "y") @?=
      Set.empty

  , testCase "free vars within lambda" $
      freeVariables "x" (lam "w" $ var "x" `call` var "y" `call` var "w" `call` var "z") @?=
      Set.fromList ["y", "z"]
  ]
