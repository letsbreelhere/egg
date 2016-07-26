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
                      let tv = TV 0
                          tv' = TV 2
                          expr = lam "x" $ lam "y" $ call (var "y") (var "x")
                          scheme = Forall [tv, tv']
                                     (TyVar tv :-> (TyVar tv :-> TyVar tv') :-> TyVar tv')
                      in testInference expr (Right scheme)
                    , testCase "infinite types" $
                      let expr = lam "x" $ call (var "x") (var "x")
                          tv = TV 0
                          ty = TyVar tv
                          ty' = TyVar (TV 1)
                      in testInference expr (Left (InfiniteType tv (ty :-> ty')))
                    , testCase "unbound variables" $
                        testInference (var "x") (Left (Unbound "x"))
                    ]

testInference :: Expr -> Either TypeError Scheme -> Assertion
testInference expr expected = runInfer (infer mempty expr) @?= expected
