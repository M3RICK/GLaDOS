module VMSpec (spec) where

import Test.Hspec
import VM.Interpreter
import IR.Types

spec :: Spec
spec = do
    describe "Stack Operations" $ do

        describe "push" $ do
            it "adds a value to the top of the stack" $ do
                let state = VMState {
                    stack = [VInt 5],
                    locals = [],
                    pc = 0,
                    callStack = []
                }
                let newState = push (VInt 10) state
                stack newState `shouldBe` [VInt 10, VInt 5]

            it "works on empty stack" $ do
                let state = VMState {
                    stack = [],
                    locals = [],
                    pc = 0,
                    callStack = []
                }
                let newState = push (VInt 42) state
                stack newState `shouldBe` [VInt 42]

        describe "pop" $ do
            it "removes and returns top value from stack" $ do
                let state = VMState {
                    stack = [VInt 10, VInt 5],
                    locals = [],
                    pc = 0,
                    callStack = []
                }
                case pop state of
                    Right (val, newState) -> do
                        val `shouldBe` VInt 10
                        stack newState `shouldBe` [VInt 5]
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

            it "returns error on empty stack" $ do
                let state = VMState {
                    stack = [],
                    locals = [],
                    pc = 0,
                    callStack = []
                }
                case pop state of
                    Left err -> err `shouldContain` "Stack underflow"
                    Right _ -> expectationFailure "Expected error but got success"

            it "works with boolean values" $ do
                let state = VMState {
                    stack = [VBool True, VInt 5],
                    locals = [],
                    pc = 0,
                    callStack = []
                }
                case pop state of
                    Right (val, newState) -> do
                        val `shouldBe` VBool True
                        stack newState `shouldBe` [VInt 5]
                    Left err -> expectationFailure $ "Unexpected error: " ++ err
