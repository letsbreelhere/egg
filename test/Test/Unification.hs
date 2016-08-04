module Test.Unification where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Types.EType
import           Types.Expr
import           Types.Constant
import           Unification

unificationSpec :: TestTree
unificationSpec = testGroup "Type unification"
                    [ testCase "constants" $
                      let lit = literal (I 12)
                      in testInference lit (Right (Forall [] (Ty "int")))
                    , testCase "identity" $
                      let a = TV 0
                      in testInference (lam "x" (var "x"))
                           (Right (Forall [a] (TyVar a :-> TyVar a)))
                    , testCase "higher-order functions" $
                      let a = TV 0
                          b = TV 1
                          expr = lam "x" $ lam "y" $ call (var "y") (var "x")
                          scheme = Forall [a, b] (TyVar a :-> (TyVar a :-> TyVar b) :-> TyVar b)
                      in testInference expr (Right scheme)
                    , testCase "infinite types" $
                      let expr = lam "x" $ call (var "x") (var "x")
                          a = TV 0
                          b = TV 1
                      in testInference expr (Left (InfiniteType a (TyVar a :-> TyVar b)))
                    , testCase "unbound variables" $
                      testInference (var "x") (Left (Unbound "x"))
                    ]

testInference :: Expr -> Either TypeError Scheme -> Assertion
testInference expr expected =
  let actual = do
                 (scheme, cs) <- runInfer mempty (infer expr)
                 su <- runSolver cs
                 pure (finalApply su scheme)
  in actual @?= expected
