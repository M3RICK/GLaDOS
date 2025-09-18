module BuiltinsSpec (spec) where

import Test.Hspec
import Data.Either (isLeft)

import Types
import Builtins
import qualified Evaluator.Core as Eval

-- Helper to unwrap successful results
expectRight :: Either String LispVal -> LispVal
expectRight (Right v) = v
expectRight (Left e)  = error $ "Unexpected error: " ++ e

spec :: Spec
spec = do
  describe "Builtins - arithmetic" $ do
    it "adds numbers" $
      expectRight (Eval.eval initialEnv (List [Atom "+", Number 2, Number 3]))
        `shouldBe` Number 5

    it "subtracts numbers" $
      expectRight (Eval.eval initialEnv (List [Atom "-", Number 7, Number 2]))
        `shouldBe` Number 5

    it "multiplies numbers" $
      expectRight (Eval.eval initialEnv (List [Atom "*", Number 4, Number 3]))
        `shouldBe` Number 12

    it "divides numbers" $
      expectRight (Eval.eval initialEnv (List [Atom "div", Number 10, Number 2]))
        `shouldBe` Number 5

    it "errors on division by zero" $
      Eval.eval initialEnv (List [Atom "div", Number 10, Number 0]) `shouldSatisfy` isLeft

    it "modulo works" $
      expectRight (Eval.eval initialEnv (List [Atom "mod", Number 10, Number 3]))
        `shouldBe` Number 1

    it "errors on modulo by zero" $
      Eval.eval initialEnv (List [Atom "mod", Number 10, Number 0]) `shouldSatisfy` isLeft


  describe "Builtins - comparisons" $ do
    it "eq? compares numbers" $
      expectRight (Eval.eval initialEnv (List [Atom "eq?", Number 4, Number 4]))
        `shouldBe` Bool True

    it "eq? compares atoms" $
      expectRight (Eval.eval initialEnv (List [Atom "eq?", Atom "foo", Atom "foo"]))
        `shouldBe` Bool True

    it "lt? works" $
      expectRight (Eval.eval initialEnv (List [Atom "<", Number 2, Number 5]))
        `shouldBe` Bool True

    it "gt? works" $
      expectRight (Eval.eval initialEnv (List [Atom ">", Number 7, Number 3]))
        `shouldBe` Bool True
