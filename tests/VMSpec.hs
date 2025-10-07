module VMSpec (spec) where

import Test.Hspec
import VM.HelperFunc
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

    describe "executeInstruction" $ do

        describe "Stack Operations" $ do
            it "executes PushInt" $ do
                let state = VMState {stack = [], locals = [], pc = 0, callStack = []}
                case executeInstruction (PushInt 42) state of
                    Right newState -> stack newState `shouldBe` [VInt 42]
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

            it "executes PushBool" $ do
                let state = VMState {stack = [], locals = [], pc = 0, callStack = []}
                case executeInstruction (PushBool True) state of
                    Right newState -> stack newState `shouldBe` [VBool True]
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

            it "executes Pop" $ do
                let state = VMState {stack = [VInt 42, VInt 10], locals = [], pc = 0, callStack = []}
                case executeInstruction Pop state of
                    Right newState -> stack newState `shouldBe` [VInt 10]
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

        describe "Variable Operations" $ do
            it "executes GetLocal" $ do
                let state = VMState {stack = [], locals = [VInt 99, VBool False], pc = 0, callStack = []}
                case executeInstruction (GetLocal 0) state of
                    Right newState -> stack newState `shouldBe` [VInt 99]
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

            it "executes SetLocal" $ do
                let state = VMState {stack = [VInt 77], locals = [VInt 0, VInt 0], pc = 0, callStack = []}
                case executeInstruction (SetLocal 1) state of
                    Right newState -> do
                        locals newState `shouldBe` [VInt 0, VInt 77]
                        stack newState `shouldBe` []  -- value was popped
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

        describe "Arithmetic Operations" $ do
            it "executes AddInt" $ do
                let state = VMState {stack = [VInt 3, VInt 5], locals = [], pc = 0, callStack = []}
                case executeInstruction AddInt state of
                    Right newState -> stack newState `shouldBe` [VInt 8]
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

            it "executes SubInt" $ do
                let state = VMState {stack = [VInt 3, VInt 10], locals = [], pc = 0, callStack = []}
                case executeInstruction SubInt state of
                    Right newState -> stack newState `shouldBe` [VInt 7]
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

            it "executes MulInt" $ do
                let state = VMState {stack = [VInt 4, VInt 7], locals = [], pc = 0, callStack = []}
                case executeInstruction MulInt state of
                    Right newState -> stack newState `shouldBe` [VInt 28]
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

            it "executes DivInt" $ do
                let state = VMState {stack = [VInt 3, VInt 15], locals = [], pc = 0, callStack = []}
                case executeInstruction DivInt state of
                    Right newState -> stack newState `shouldBe` [VInt 5]
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

            it "returns error on division by zero" $ do
                let state = VMState {stack = [VInt 0, VInt 10], locals = [], pc = 0, callStack = []}
                case executeInstruction DivInt state of
                    Left err -> err `shouldContain` "Division by zero"
                    Right _ -> expectationFailure "Expected division by zero error"

        describe "Comparison Operations" $ do
            it "executes EqInt (true)" $ do
                let state = VMState {stack = [VInt 5, VInt 5], locals = [], pc = 0, callStack = []}
                case executeInstruction EqInt state of
                    Right newState -> stack newState `shouldBe` [VBool True]
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

            it "executes EqInt (false)" $ do
                let state = VMState {stack = [VInt 3, VInt 5], locals = [], pc = 0, callStack = []}
                case executeInstruction EqInt state of
                    Right newState -> stack newState `shouldBe` [VBool False]
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

            it "executes LtInt" $ do
                let state = VMState {stack = [VInt 7, VInt 5], locals = [], pc = 0, callStack = []}
                case executeInstruction LtInt state of
                    Right newState -> stack newState `shouldBe` [VBool True]
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

        describe "Logical Operations" $ do
            it "executes AndBool (true && false)" $ do
                let state = VMState {stack = [VBool False, VBool True], locals = [], pc = 0, callStack = []}
                case executeInstruction AndBool state of
                    Right newState -> stack newState `shouldBe` [VBool False]
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

            it "executes OrBool (true || false)" $ do
                let state = VMState {stack = [VBool False, VBool True], locals = [], pc = 0, callStack = []}
                case executeInstruction OrBool state of
                    Right newState -> stack newState `shouldBe` [VBool True]
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

        describe "Control Flow Operations" $ do
            it "executes Jump" $ do
                let state = VMState {stack = [], locals = [], pc = 5, callStack = []}
                case executeInstruction (Jump 10) state of
                    Right newState -> pc newState `shouldBe` 10
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

            it "executes JumpIfFalse (condition is false, should jump)" $ do
                let state = VMState {stack = [VBool False], locals = [], pc = 5, callStack = []}
                case executeInstruction (JumpIfFalse 20) state of
                    Right newState -> do
                        pc newState `shouldBe` 20
                        stack newState `shouldBe` []  -- bool was popped
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

            it "executes JumpIfFalse (condition is true, should not jump)" $ do
                let state = VMState {stack = [VBool True], locals = [], pc = 5, callStack = []}
                case executeInstruction (JumpIfFalse 20) state of
                    Right newState -> do
                        pc newState `shouldBe` 5  -- PC unchanged
                        stack newState `shouldBe` []  -- bool was popped
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

            it "executes Halt" $ do
                let state = VMState {stack = [VInt 42], locals = [], pc = 10, callStack = []}
                case executeInstruction Halt state of
                    Right newState -> do
                        stack newState `shouldBe` [VInt 42]
                        pc newState `shouldBe` 10
                    Left err -> expectationFailure $ "Unexpected error: " ++ err

    describe "execute (main loop)" $ do

        it "executes a simple program: 5 + 3" $ do
            let func = CompiledFunction {
                funcName = "main",
                paramCount = 0,
                localVarCount = 0,
                code = [PushInt 5, PushInt 3, AddInt, Halt]
            }
            let program = IRProgram {functions = [func], mainIndex = 0}
            case execute program 0 [] of
                Right result -> result `shouldBe` VInt 8
                Left err -> expectationFailure $ "Unexpected error: " ++ err

        it "executes with local variables" $ do
            let func = CompiledFunction {
                funcName = "test",
                paramCount = 2,
                localVarCount = 3,
                code = [GetLocal 0, GetLocal 1, AddInt, SetLocal 2, GetLocal 2, Halt]
            }
            let program = IRProgram {functions = [func], mainIndex = 0}
            case execute program 0 [VInt 10, VInt 20] of
                Right result -> result `shouldBe` VInt 30
                Left err -> expectationFailure $ "Unexpected error: " ++ err

        it "executes with Jump" $ do
            let func = CompiledFunction {
                funcName = "jump_test",
                paramCount = 0,
                localVarCount = 0,
                code = [PushInt 1, Jump 3, PushInt 999, PushInt 2, AddInt, Halt]
            }
            let program = IRProgram {functions = [func], mainIndex = 0}
            case execute program 0 [] of
                Right result -> result `shouldBe` VInt 3
                Left err -> expectationFailure $ "Unexpected error: " ++ err

        it "executes JumpIfFalse (true, don't jump)" $ do
            let func = CompiledFunction {
                funcName = "if_test",
                paramCount = 0,
                localVarCount = 0,
                code = [PushBool True, JumpIfFalse 4, PushInt 42, Jump 5, PushInt 999, Halt]
            }
            let program = IRProgram {functions = [func], mainIndex = 0}
            case execute program 0 [] of
                Right result -> result `shouldBe` VInt 42
                Left err -> expectationFailure $ "Unexpected error: " ++ err

        it "executes JumpIfFalse (false, do jump)" $ do
            let func = CompiledFunction {
                funcName = "if_test2",
                paramCount = 0,
                localVarCount = 0,
                code = [PushBool False, JumpIfFalse 4, PushInt 999, Jump 5, PushInt 100, Halt]
            }
            let program = IRProgram {functions = [func], mainIndex = 0}
            case execute program 0 [] of
                Right result -> result `shouldBe` VInt 100
                Left err -> expectationFailure $ "Unexpected error: " ++ err

        it "returns error on division by zero" $ do
            let func = CompiledFunction {
                funcName = "div_zero",
                paramCount = 0,
                localVarCount = 0,
                code = [PushInt 10, PushInt 0, DivInt, Halt]
            }
            let program = IRProgram {functions = [func], mainIndex = 0}
            case execute program 0 [] of
                Left err -> err `shouldContain` "Division by zero"
                Right _ -> expectationFailure "Expected division by zero error"

        it "returns error when halting with empty stack" $ do
            let func = CompiledFunction {
                funcName = "empty_halt",
                paramCount = 0,
                localVarCount = 0,
                code = [Halt]
            }
            let program = IRProgram {functions = [func], mainIndex = 0}
            case execute program 0 [] of
                Left err -> err `shouldContain` "empty stack"
                Right _ -> expectationFailure "Expected empty stack error"
