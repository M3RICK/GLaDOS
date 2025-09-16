module EvaluatorSpec (spec) where

import Test.Hspec
import Data.Either (isLeft)

import Types
import Environment
import qualified Evaluator.Core as Eval

-- Helper for extracting Right values in tests
expectRight :: Either String LispVal -> LispVal
expectRight (Right v) = v
expectRight (Left e)  = error $ "Unexpected error: " ++ e

-- A dummy environment with a builtin +
testEnv :: Env
testEnv = defineVar "+" (Builtin add) emptyEnv
  where
    add [Number a, Number b] = Right (Number (a + b))
    add args = Left $ "*** ERROR: invalid args to +: " ++ show args

spec :: Spec
spec = do
  describe "Evaluator - base cases" $ do
    it "evaluates numbers to themselves" $
      expectRight (Eval.eval emptyEnv (Number 42)) `shouldBe` Number 42

    it "evaluates booleans to themselves" $
      expectRight (Eval.eval emptyEnv (Bool True)) `shouldBe` Bool True

    it "looks up bound variables" $
      let env = defineVar "x" (Number 99) emptyEnv
      in expectRight (Eval.eval env (Atom "x")) `shouldBe` Number 99

    it "errors on unbound variables" $
      Eval.eval emptyEnv (Atom "y") `shouldSatisfy` isLeft


  describe "Evaluator - define" $ do
    it "binds a variable" $
      case Eval.eval emptyEnv (List [Atom "define", Atom "foo", Number 7]) of
        Right (Function [] (Atom "foo") env') ->
          expectRight (Eval.eval env' (Atom "foo")) `shouldBe` Number 7
        bad -> error $ "Unexpected define result: " ++ show bad


  describe "Evaluator - if" $ do
    it "chooses then-branch when condition is true" $
      expectRight (Eval.eval emptyEnv (List [Atom "if", Bool True, Number 1, Number 2]))
        `shouldBe` Number 1

    it "chooses else-branch when condition is false" $
      expectRight (Eval.eval emptyEnv (List [Atom "if", Bool False, Number 1, Number 2]))
        `shouldBe` Number 2


  describe "Evaluator - lambda and application" $ do
    it "creates a lambda" $
      case Eval.eval emptyEnv (List [Atom "lambda", List [Atom "x"], Atom "x"]) of
        Right (Function ["x"] (Atom "x") _) -> True `shouldBe` True
        bad -> error $ "Unexpected lambda result: " ++ show bad

    it "applies a builtin function" $
      expectRight (Eval.eval testEnv (List [Atom "+", Number 2, Number 3]))
        `shouldBe` Number 5

    it "applies a user-defined function" $
      let fn = List [Atom "lambda", List [Atom "x"], List [Atom "+", Atom "x", Number 1]]
      in expectRight (Eval.eval testEnv (List [fn, Number 4]))
        `shouldBe` Number 5

    it "fails when applying non-functions" $
      Eval.eval testEnv (List [Number 10, Number 20]) `shouldSatisfy` isLeft
