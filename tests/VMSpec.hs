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

    describe "Local Variable Operations" $ do

        describe "getLocal" $ do
            it "retrieves value at index 0" $ do
                let state = VMState {
                    stack = [],
                    locals = [VInt 42, VInt 10],
                    pc = 0,
                    callStack = []
                }
                case getLocal 0 state of
                    Right val -> val `shouldBe` VInt 42
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

            it "retrieves value at index 1" $ do
                let state = VMState {
                    stack = [],
                    locals = [VInt 42, VInt 10, VBool True],
                    pc = 0,
                    callStack = []
                }
                case getLocal 1 state of
                    Right val -> val `shouldBe` VInt 10
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

            it "returns error for negative index" $ do
                let state = VMState {
                    stack = [],
                    locals = [VInt 42],
                    pc = 0,
                    callStack = []
                }
                case getLocal (-1) state of
                    Left err -> err `shouldContain` "negative"
                    Right _ -> expectationFailure "Expected error but got success"

            it "returns error for out of bounds index" $ do
                let state = VMState {
                    stack = [],
                    locals = [VInt 42, VInt 10],
                    pc = 0,
                    callStack = []
                }
                case getLocal 5 state of
                    Left err -> err `shouldContain` "out of bounds"
                    Right _ -> expectationFailure "Expected error but got success"

        describe "setLocal" $ do
            it "updates value at index 0" $ do
                let state = VMState {
                    stack = [],
                    locals = [VInt 42, VInt 10],
                    pc = 0,
                    callStack = []
                }
                case setLocal 0 (VInt 99) state of
                    Right newState -> locals newState `shouldBe` [VInt 99, VInt 10]
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

            it "updates value at index 1" $ do
                let state = VMState {
                    stack = [],
                    locals = [VInt 42, VInt 10, VBool False],
                    pc = 0,
                    callStack = []
                }
                case setLocal 1 (VBool True) state of
                    Right newState -> locals newState `shouldBe` [VInt 42, VBool True, VBool False]
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

            it "returns error for negative index" $ do
                let state = VMState {
                    stack = [],
                    locals = [VInt 42],
                    pc = 0,
                    callStack = []
                }
                case setLocal (-1) (VInt 99) state of
                    Left err -> err `shouldContain` "negative"
                    Right _ -> expectationFailure "Expected error but got success"

            it "returns error for out of bounds index" $ do
                let state = VMState {
                    stack = [],
                    locals = [VInt 42, VInt 10],
                    pc = 0,
                    callStack = []
                }
                case setLocal 5 (VInt 99) state of
                    Left err -> err `shouldContain` "out of bounds"
                    Right _ -> expectationFailure "Expected error but got success"

    describe "Type-Safe Pop Operations" $ do

        describe "popInt" $ do
            it "pops an integer successfully" $ do
                let state = VMState {
                    stack = [VInt 42, VInt 10],
                    locals = [],
                    pc = 0,
                    callStack = []
                }
                case popInt state of
                    Right (val, newState) -> do
                        val `shouldBe` 42
                        stack newState `shouldBe` [VInt 10]
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

            it "returns error when popping Bool instead of Int" $ do
                let state = VMState {
                    stack = [VBool True],
                    locals = [],
                    pc = 0,
                    callStack = []
                }
                case popInt state of
                    Left err -> err `shouldContain` "Type error"
                    Right _ -> expectationFailure "Expected type error"

            it "returns error on empty stack" $ do
                let state = VMState {
                    stack = [],
                    locals = [],
                    pc = 0,
                    callStack = []
                }
                case popInt state of
                    Left err -> err `shouldContain` "Stack underflow"
                    Right _ -> expectationFailure "Expected error"

        describe "popBool" $ do
            it "pops a boolean successfully" $ do
                let state = VMState {
                    stack = [VBool True, VInt 5],
                    locals = [],
                    pc = 0,
                    callStack = []
                }
                case popBool state of
                    Right (val, newState) -> do
                        val `shouldBe` True
                        stack newState `shouldBe` [VInt 5]
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

            it "returns error when popping Int instead of Bool" $ do
                let state = VMState {
                    stack = [VInt 42],
                    locals = [],
                    pc = 0,
                    callStack = []
                }
                case popBool state of
                    Left err -> err `shouldContain` "Type error"
                    Right _ -> expectationFailure "Expected type error"

    describe "Binary Operations" $ do

        describe "binaryIntOp" $ do
            it "performs addition" $ do
                let state = VMState {
                    stack = [VInt 3, VInt 5],  -- top is 3, second is 5
                    locals = [],
                    pc = 0,
                    callStack = []
                }
                case binaryIntOp (+) state of
                    Right newState -> stack newState `shouldBe` [VInt 8]  -- 5 + 3
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

            it "performs subtraction with correct order" $ do
                let state = VMState {
                    stack = [VInt 3, VInt 10],  -- top is 3, second is 10
                    locals = [],
                    pc = 0,
                    callStack = []
                }
                case binaryIntOp (-) state of
                    Right newState -> stack newState `shouldBe` [VInt 7]  -- 10 - 3
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

            it "returns error when first operand is Bool" $ do
                let state = VMState {
                    stack = [VBool True, VInt 5],
                    locals = [],
                    pc = 0,
                    callStack = []
                }
                case binaryIntOp (+) state of
                    Left err -> err `shouldContain` "Type error"
                    Right _ -> expectationFailure "Expected type error"

            it "returns error when second operand is Bool" $ do
                let state = VMState {
                    stack = [VInt 3, VBool False],
                    locals = [],
                    pc = 0,
                    callStack = []
                }
                case binaryIntOp (+) state of
                    Left err -> err `shouldContain` "Type error"
                    Right _ -> expectationFailure "Expected type error"

        describe "compareInts" $ do
            it "compares with less than" $ do
                let state = VMState {
                    stack = [VInt 7, VInt 5],  -- 5 < 7
                    locals = [],
                    pc = 0,
                    callStack = []
                }
                case compareInts (<) state of
                    Right newState -> stack newState `shouldBe` [VBool True]
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

            it "compares with equality" $ do
                let state = VMState {
                    stack = [VInt 5, VInt 5],
                    locals = [],
                    pc = 0,
                    callStack = []
                }
                case compareInts (==) state of
                    Right newState -> stack newState `shouldBe` [VBool True]
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

        describe "binaryBoolOp" $ do
            it "performs logical AND" $ do
                let state = VMState {
                    stack = [VBool False, VBool True],  -- True && False
                    locals = [],
                    pc = 0,
                    callStack = []
                }
                case binaryBoolOp (&&) state of
                    Right newState -> stack newState `shouldBe` [VBool False]
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

            it "performs logical OR" $ do
                let state = VMState {
                    stack = [VBool False, VBool True],  -- True || False
                    locals = [],
                    pc = 0,
                    callStack = []
                }
                case binaryBoolOp (||) state of
                    Right newState -> stack newState `shouldBe` [VBool True]
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

            it "returns error when first operand is Int" $ do
                let state = VMState {
                    stack = [VInt 5, VBool True],
                    locals = [],
                    pc = 0,
                    callStack = []
                }
                case binaryBoolOp (&&) state of
                    Left err -> err `shouldContain` "Type error"
                    Right _ -> expectationFailure "Expected type error"
