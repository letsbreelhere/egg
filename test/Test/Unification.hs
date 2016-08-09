module Test.Unification where

import           Control.Comonad.Cofree
import           Test.Tasty
import           Test.Tasty.HUnit
import           Types.Constant
import           Types.EType
import           Types.Expr
import           Unification
import           Unification.Substitutable (freeTyVars)
import qualified Data.Set as Set

unificationSpec :: TestTree
unificationSpec = testGroup "Type unification"
                    [ testCase "constants" $
                      let lit = literal (I 12)
                      in testInference lit (Right $ Forall [] (Ty "int"))
                    , testCase "identity" $
                      let a = TV 0
                          t = TyVar a
                      in testInference (lam "x" (var "x")) (Right $ Forall [a] (t :-> t))
                    , testCase "higher-order functions" $
                      let a = TyVar $ TV 0
                          b = TyVar $ TV 1
                          expr = lam "x" $ lam "y" $ call (var "y") (var "x")
                          ty = a :-> (a :-> b) :-> b
                      in testInference expr (Right $ Forall [TV 0, TV 1] ty)
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
                 (t :< _, cs) <- runInfer mempty (infer expr)
                 su <- runSolver cs
                 a <- pure (apply su t)
                 pure $ Forall (Set.toList $ freeTyVars a) a
  in actual @?= expected
