module ErrorSpec (spec) where

import Test.Hspec
import Parser.Core (parseProgram)
import Security.TypeChecker (checkProgram)
import Compiler.Core (compileProgram)
import Bytecode.Serialize (serializeProgram, deserializeProgram, saveProgramToFile, loadProgramFromFile)
import VM.Interpreter (execute)
import IR.Types

spec :: Spec
spec = do

-- ============================================================================
-- Parser Error Tests
-- ============================================================================

    describe "Parser Errors" $ do

        it "detects invalid expression syntax" $ do
            let source = "int main() { int x = ; }"
            case parseProgram source of
                Left _ -> return ()  -- Any parse error is expected
                Right _ -> expectationFailure "Expected parse error"

        it "detects missing closing brace" $ do
            let source = "int main() { int x = 5; return x;"
            case parseProgram source of
                Left _ -> return ()  -- Any parse error is expected
                Right _ -> expectationFailure "Expected parse error"

        it "detects missing closing brace in function body" $ do
            let source = "int main() { return 0;"
            case parseProgram source of
                Left _ -> return ()  -- Any parse error is expected
                Right _ -> expectationFailure "Expected parse error"

        it "detects invalid function parameter syntax" $ do
            let source = "int foo(int,) { return 0; }"
            case parseProgram source of
                Left _ -> return ()  -- Any parse error is expected
                Right _ -> expectationFailure "Expected parse error"

-- ============================================================================
-- Type Checker Error Tests
-- ============================================================================

    describe "Type Checker Errors" $ do

        it "detects type mismatch in binary operation (int + bool)" $ do
            let source = "int main() { int x = 5; return x + true; }"
            case parseProgram source of
                Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                Right ast -> case checkProgram ast of
                    Left typeErrs -> length typeErrs `shouldSatisfy` (> 0)
                    Right _ -> expectationFailure "Expected type error"

        it "detects type mismatch in assignment (int = bool)" $ do
            let source = "int main() { int x = true; return x; }"
            case parseProgram source of
                Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                Right ast -> case checkProgram ast of
                    Left typeErrs -> length typeErrs `shouldSatisfy` (> 0)
                    Right _ -> expectationFailure "Expected type error"

        it "detects type mismatch in return statement" $ do
            let source = "int foo() { return true; }"
            case parseProgram source of
                Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                Right ast -> case checkProgram ast of
                    Left typeErrs -> length typeErrs `shouldSatisfy` (> 0)
                    Right _ -> expectationFailure "Expected type error"

        it "detects boolean operation on integers" $ do
            let source = "int main() { int x = 5; bool y = x && true; return 0; }"
            case parseProgram source of
                Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                Right ast -> case checkProgram ast of
                    Left typeErrs -> length typeErrs `shouldSatisfy` (> 0)
                    Right _ -> expectationFailure "Expected type error"

        it "detects arithmetic operation on booleans" $ do
            let source = "int main() { bool x = true; int y = x + 5; return 0; }"
            case parseProgram source of
                Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                Right ast -> case checkProgram ast of
                    Left typeErrs -> length typeErrs `shouldSatisfy` (> 0)
                    Right _ -> expectationFailure "Expected type error"

        it "detects comparison between incompatible types" $ do
            let source = "int main() { bool x = 5 < true; return 0; }"
            case parseProgram source of
                Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                Right ast -> case checkProgram ast of
                    Left typeErrs -> length typeErrs `shouldSatisfy` (> 0)
                    Right _ -> expectationFailure "Expected type error"

        it "detects missing return statement" $ do
            let source = "int foo() { int x = 5; }"
            case parseProgram source of
                Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                Right ast -> case checkProgram ast of
                    Left typeErrs -> length typeErrs `shouldSatisfy` (> 0)
                    Right _ -> expectationFailure "Expected type error for missing return"

        it "detects float/int type mismatch" $ do
            let source = "int main() { float x = 3.14; int y = x; return y; }"
            case parseProgram source of
                Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                Right ast -> case checkProgram ast of
                    Left typeErrs -> length typeErrs `shouldSatisfy` (> 0)
                    Right _ -> expectationFailure "Expected type error"

-- ============================================================================
-- Compiler Error Tests
-- ============================================================================

    describe "Compiler Errors" $ do

        it "compiles valid program without errors" $ do
            let source = "int main() { int x = 5; return x + 10; }"
            case parseProgram source of
                Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                Right ast -> case checkProgram ast of
                    Left typeErrs -> expectationFailure $ "Type check failed: " ++ show typeErrs
                    Right validatedAst -> case compileProgram validatedAst of
                        Left compileErr -> expectationFailure $ "Compile failed: " ++ show compileErr
                        Right _ -> return ()  -- Success

        it "handles nested expressions correctly" $ do
            let source = "int main() { return ((5 + 3) * 2) - 1; }"
            case parseProgram source of
                Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                Right ast -> case checkProgram ast of
                    Left typeErrs -> expectationFailure $ "Type check failed: " ++ show typeErrs
                    Right validatedAst -> case compileProgram validatedAst of
                        Left compileErr -> expectationFailure $ "Compile failed: " ++ show compileErr
                        Right irProgram -> case execute irProgram (mainIndex irProgram) [] of
                            Left runtimeErr -> expectationFailure $ "Runtime error: " ++ runtimeErr
                            Right result -> result `shouldBe` VInt 15

        it "handles boolean expressions" $ do
            let source = "int main() { bool x = true && false; if (x) { return 1; } return 0; }"
            case parseProgram source of
                Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                Right ast -> case checkProgram ast of
                    Left typeErrs -> expectationFailure $ "Type check failed: " ++ show typeErrs
                    Right validatedAst -> case compileProgram validatedAst of
                        Left compileErr -> expectationFailure $ "Compile failed: " ++ show compileErr
                        Right _ -> return ()  -- Success

        it "handles function calls" $ do
            let source = "int add(int a, int b) { return a + b; } int main() { return add(5, 3); }"
            case parseProgram source of
                Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                Right ast -> case checkProgram ast of
                    Left typeErrs -> expectationFailure $ "Type check failed: " ++ show typeErrs
                    Right validatedAst -> case compileProgram validatedAst of
                        Left compileErr -> expectationFailure $ "Compile failed: " ++ show compileErr
                        Right irProgram -> case execute irProgram (mainIndex irProgram) [] of
                            Left runtimeErr -> expectationFailure $ "Runtime error: " ++ runtimeErr
                            Right result -> result `shouldBe` VInt 8

        it "handles local variables correctly" $ do
            let source = "int main() { int x = 5; int y = 10; int z = x + y; return z; }"
            case parseProgram source of
                Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                Right ast -> case checkProgram ast of
                    Left typeErrs -> expectationFailure $ "Type check failed: " ++ show typeErrs
                    Right validatedAst -> case compileProgram validatedAst of
                        Left compileErr -> expectationFailure $ "Compile failed: " ++ show compileErr
                        Right irProgram -> case execute irProgram (mainIndex irProgram) [] of
                            Left runtimeErr -> expectationFailure $ "Runtime error: " ++ runtimeErr
                            Right result -> result `shouldBe` VInt 15

        it "handles if-else statements" $ do
            let source = "int main() { if (true) { return 42; } else { return 99; } }"
            case parseProgram source of
                Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                Right ast -> case checkProgram ast of
                    Left typeErrs -> expectationFailure $ "Type check failed: " ++ show typeErrs
                    Right validatedAst -> case compileProgram validatedAst of
                        Left compileErr -> expectationFailure $ "Compile failed: " ++ show compileErr
                        Right irProgram -> case execute irProgram (mainIndex irProgram) [] of
                            Left runtimeErr -> expectationFailure $ "Runtime error: " ++ runtimeErr
                            Right result -> result `shouldBe` VInt 42

        it "handles while loops" $ do
            let source = "int main() { int x = 0; while (x < 5) { x = x + 1; } return x; }"
            case parseProgram source of
                Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                Right ast -> case checkProgram ast of
                    Left typeErrs -> expectationFailure $ "Type check failed: " ++ show typeErrs
                    Right validatedAst -> case compileProgram validatedAst of
                        Left compileErr -> expectationFailure $ "Compile failed: " ++ show compileErr
                        Right irProgram -> case execute irProgram (mainIndex irProgram) [] of
                            Left runtimeErr -> expectationFailure $ "Runtime error: " ++ runtimeErr
                            Right result -> result `shouldBe` VInt 5

-- ============================================================================
-- Bytecode Serialization Error Tests
-- ============================================================================

    describe "Bytecode Serialization Errors" $ do

        it "successfully serializes and deserializes valid program" $ do
            let func = CompiledFunction {
                funcName = "test",
                paramCount = 0,
                localVarCount = 0,
                code = [PushInt 42, Halt]
            }
            let program = IRProgram {functions = [func], mainIndex = 0}
            let bytes = serializeProgram program
            case deserializeProgram bytes of
                Left err -> expectationFailure $ "Deserialization failed: " ++ err
                Right loadedProgram -> do
                    funcName (head (functions loadedProgram)) `shouldBe` "test"
                    code (head (functions loadedProgram)) `shouldBe` [PushInt 42, Halt]

        it "handles file I/O errors gracefully" $ do
            -- Try to load from non-existent file
            result <- loadProgramFromFile "/nonexistent/path/file.gbc"
            case result of
                Left err -> err `shouldContain` "Failed to read file"
                Right _ -> expectationFailure "Expected file read error"

        it "saves and loads bytecode file correctly" $ do
            let func = CompiledFunction {
                funcName = "add",
                paramCount = 2,
                localVarCount = 2,
                code = [GetLocal 0, GetLocal 1, AddInt, Return]
            }
            let program = IRProgram {functions = [func], mainIndex = 0}
            let testFile = "/tmp/errorspec_test.gbc"
            saveResult <- saveProgramToFile testFile program
            case saveResult of
                Left err -> expectationFailure $ "Failed to save: " ++ err
                Right () -> do
                    loadResult <- loadProgramFromFile testFile
                    case loadResult of
                        Left err -> expectationFailure $ "Failed to load: " ++ err
                        Right loadedProgram -> do
                            funcName (head (functions loadedProgram)) `shouldBe` "add"
                            paramCount (head (functions loadedProgram)) `shouldBe` 2

-- ============================================================================
-- Runtime/VM Error Tests
-- ============================================================================

    describe "Runtime/VM Errors" $ do

        it "detects division by zero" $ do
            let func = CompiledFunction {
                funcName = "main",
                paramCount = 0,
                localVarCount = 0,
                code = [PushInt 10, PushInt 0, DivInt, Halt]
            }
            let program = IRProgram {functions = [func], mainIndex = 0}
            case execute program 0 [] of
                Left err -> err `shouldContain` "Division by zero"
                Right _ -> expectationFailure "Expected division by zero error"

        it "detects stack underflow" $ do
            let func = CompiledFunction {
                funcName = "main",
                paramCount = 0,
                localVarCount = 0,
                code = [AddInt, Halt]  -- Try to add without pushing values
            }
            let program = IRProgram {functions = [func], mainIndex = 0}
            case execute program 0 [] of
                Left err -> err `shouldContain` "Stack underflow"
                Right _ -> expectationFailure "Expected stack underflow error"

        it "detects halt with empty stack" $ do
            let func = CompiledFunction {
                funcName = "main",
                paramCount = 0,
                localVarCount = 0,
                code = [Halt]  -- Halt without pushing return value
            }
            let program = IRProgram {functions = [func], mainIndex = 0}
            case execute program 0 [] of
                Left err -> err `shouldContain` "empty stack"
                Right _ -> expectationFailure "Expected empty stack error"

        it "detects type mismatch in VM operations (bool instead of int)" $ do
            let func = CompiledFunction {
                funcName = "main",
                paramCount = 0,
                localVarCount = 0,
                code = [PushBool True, PushInt 5, AddInt, Halt]
            }
            let program = IRProgram {functions = [func], mainIndex = 0}
            case execute program 0 [] of
                Left err -> err `shouldContain` "Type error"
                Right _ -> expectationFailure "Expected type error"

        it "detects invalid local variable access (negative index)" $ do
            let func = CompiledFunction {
                funcName = "main",
                paramCount = 0,
                localVarCount = 1,
                code = [GetLocal (-1), Halt]
            }
            let program = IRProgram {functions = [func], mainIndex = 0}
            case execute program 0 [] of
                Left err -> err `shouldContain` "negative"
                Right _ -> expectationFailure "Expected negative index error"

        it "detects invalid local variable access (out of bounds)" $ do
            let func = CompiledFunction {
                funcName = "main",
                paramCount = 0,
                localVarCount = 2,
                code = [GetLocal 5, Halt]  -- Only have 2 locals, accessing index 5
            }
            let program = IRProgram {functions = [func], mainIndex = 0}
            case execute program 0 [] of
                Left err -> err `shouldContain` "out of bounds"
                Right _ -> expectationFailure "Expected out of bounds error"

-- ============================================================================
-- End-to-End Error Tests (Full Pipeline)
-- ============================================================================

    describe "End-to-End Error Tests" $ do

        it "propagates parse error through full pipeline" $ do
            let source = "int main() { int x = ; }"
            case parseProgram source of
                Left _ -> return ()  -- Expected
                Right _ -> expectationFailure "Expected parse error"

        it "propagates type error through full pipeline" $ do
            let source = "int main() { return true; }"
            case parseProgram source of
                Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                Right ast -> case checkProgram ast of
                    Left _ -> return ()  -- Expected type error
                    Right _ -> expectationFailure "Expected type error"

        it "successfully executes valid program through full pipeline" $ do
            let source = "int main() { int x = 5; return x + 10; }"
            case parseProgram source of
                Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                Right ast -> case checkProgram ast of
                    Left typeErrs -> expectationFailure $ "Type check failed: " ++ show typeErrs
                    Right validatedAst -> case compileProgram validatedAst of
                        Left compileErr -> expectationFailure $ "Compile failed: " ++ show compileErr
                        Right irProgram -> case execute irProgram (mainIndex irProgram) [] of
                            Left runtimeErr -> expectationFailure $ "Runtime error: " ++ runtimeErr
                            Right result -> result `shouldBe` VInt 15

        it "compiles to bytecode and executes successfully" $ do
            let source = "int main() { return 42; }"
            case parseProgram source of
                Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                Right ast -> case checkProgram ast of
                    Left typeErrs -> expectationFailure $ "Type check failed: " ++ show typeErrs
                    Right validatedAst -> case compileProgram validatedAst of
                        Left compileErr -> expectationFailure $ "Compile failed: " ++ show compileErr
                        Right irProgram -> do
                            let bytes = serializeProgram irProgram
                            case deserializeProgram bytes of
                                Left err -> expectationFailure $ "Deserialization failed: " ++ err
                                Right loadedProgram -> case execute loadedProgram (mainIndex loadedProgram) [] of
                                    Left runtimeErr -> expectationFailure $ "Runtime error: " ++ runtimeErr
                                    Right result -> result `shouldBe` VInt 42
