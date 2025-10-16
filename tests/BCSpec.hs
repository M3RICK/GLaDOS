module BCSpec (spec) where

import Test.Hspec
import Data.Binary (encode, decode)
import qualified Data.ByteString.Lazy as BSL
import Bytecode.Serialize
import Bytecode.BcTypes ()
import IR.Types

-- Helper: Serialize and deserialize (round-trip test)
roundTrip :: IRProgram -> IRProgram
roundTrip program = case deserializeProgram (serializeProgram program) of
    Right p -> p
    Left err -> error $ "Round trip failed: " ++ err

spec :: Spec
spec = do

-- ============================================================================
-- Serialization Tests
-- ============================================================================

    describe "Serialization" $ do

        describe "serializeProgram" $ do
            it "produces non-empty ByteString" $ do
                let func = CompiledFunction {
                    funcName = "main",
                    paramCount = 0,
                    localVarCount = 0,
                    code = [PushInt 42, Halt]
                }
                let program = IRProgram {functions = [func], mainIndex = 0}
                let bytes = serializeProgram program
                BSL.length bytes `shouldSatisfy` (> 0)

            it "starts with HELL magic number" $ do
                let func = CompiledFunction {
                    funcName = "test",
                    paramCount = 0,
                    localVarCount = 0,
                    code = [Halt]
                }
                let program = IRProgram {functions = [func], mainIndex = 0}
                let bytes = serializeProgram program
                let header = BSL.take 4 bytes
                header `shouldBe` BSL.pack [0x48, 0x45, 0x4c, 0x4c]

-- ============================================================================
-- Deserialization Tests
-- ============================================================================

    describe "Deserialization" $ do

        describe "deserializeProgram" $ do
            it "deserializes a simple program" $ do
                let func = CompiledFunction {
                    funcName = "main",
                    paramCount = 0,
                    localVarCount = 0,
                    code = [PushInt 10, Halt]
                }
                let program = IRProgram {functions = [func], mainIndex = 0}
                let bytes = serializeProgram program
                case deserializeProgram bytes of
                    Right result -> funcName (head (functions result)) `shouldBe` "main"
                    Left err -> error $ "Deserialization failed: " ++ err

            it "deserializes program with multiple functions" $ do
                let func1 = CompiledFunction {
                    funcName = "add",
                    paramCount = 2,
                    localVarCount = 2,
                    code = [GetLocal 0, GetLocal 1, AddInt, Return]
                }
                let func2 = CompiledFunction {
                    funcName = "main",
                    paramCount = 0,
                    localVarCount = 0,
                    code = [PushInt 5, PushInt 3, Call 0, Halt]
                }
                let program = IRProgram {functions = [func1, func2], mainIndex = 1}
                let bytes = serializeProgram program
                case deserializeProgram bytes of
                    Right result -> length (functions result) `shouldBe` 2
                    Left err -> error $ "Deserialization failed: " ++ err

-- ============================================================================
-- Round-Trip Tests
-- ============================================================================

    describe "Round-Trip Tests" $ do

        describe "Instructions" $ do
            it "round-trips PushInt instruction" $ do
                let func = CompiledFunction {
                    funcName = "test",
                    paramCount = 0,
                    localVarCount = 0,
                    code = [PushInt 42]
                }
                let program = IRProgram {functions = [func], mainIndex = 0}
                let result = roundTrip program
                code (head (functions result)) `shouldBe` [PushInt 42]

            it "round-trips PushBool instruction" $ do
                let func = CompiledFunction {
                    funcName = "test",
                    paramCount = 0,
                    localVarCount = 0,
                    code = [PushBool True]
                }
                let program = IRProgram {functions = [func], mainIndex = 0}
                let result = roundTrip program
                code (head (functions result)) `shouldBe` [PushBool True]

            it "round-trips arithmetic instructions" $ do
                let func = CompiledFunction {
                    funcName = "test",
                    paramCount = 0,
                    localVarCount = 0,
                    code = [AddInt, SubInt, MulInt, DivInt]
                }
                let program = IRProgram {functions = [func], mainIndex = 0}
                let result = roundTrip program
                code (head (functions result)) `shouldBe` [AddInt, SubInt, MulInt, DivInt]

            it "round-trips comparison instructions" $ do
                let func = CompiledFunction {
                    funcName = "test",
                    paramCount = 0,
                    localVarCount = 0,
                    code = [EqInt, NeqInt, LtInt, GtInt, LeInt, GeInt]
                }
                let program = IRProgram {functions = [func], mainIndex = 0}
                let result = roundTrip program
                code (head (functions result)) `shouldBe` [EqInt, NeqInt, LtInt, GtInt, LeInt, GeInt]

            it "round-trips logical instructions" $ do
                let func = CompiledFunction {
                    funcName = "test",
                    paramCount = 0,
                    localVarCount = 0,
                    code = [AndBool, OrBool]
                }
                let program = IRProgram {functions = [func], mainIndex = 0}
                let result = roundTrip program
                code (head (functions result)) `shouldBe` [AndBool, OrBool]

            it "round-trips control flow instructions" $ do
                let func = CompiledFunction {
                    funcName = "test",
                    paramCount = 0,
                    localVarCount = 0,
                    code = [Jump 5, JumpIfFalse 10, Call 2, Return, Halt]
                }
                let program = IRProgram {functions = [func], mainIndex = 0}
                let result = roundTrip program
                code (head (functions result)) `shouldBe` [Jump 5, JumpIfFalse 10, Call 2, Return, Halt]

            it "round-trips variable instructions" $ do
                let func = CompiledFunction {
                    funcName = "test",
                    paramCount = 0,
                    localVarCount = 2,
                    code = [GetLocal 0, SetLocal 1, GetLocal 1]
                }
                let program = IRProgram {functions = [func], mainIndex = 0}
                let result = roundTrip program
                code (head (functions result)) `shouldBe` [GetLocal 0, SetLocal 1, GetLocal 1]

            it "round-trips PushFloat instruction" $ do
                let func = CompiledFunction {
                    funcName = "test",
                    paramCount = 0,
                    localVarCount = 0,
                    code = [PushFloat 3.14]
                }
                let program = IRProgram {functions = [func], mainIndex = 0}
                let result = roundTrip program
                code (head (functions result)) `shouldBe` [PushFloat 3.14]

            it "round-trips float arithmetic instructions" $ do
                let func = CompiledFunction {
                    funcName = "test",
                    paramCount = 0,
                    localVarCount = 0,
                    code = [AddFloat, SubFloat, MulFloat, DivFloat]
                }
                let program = IRProgram {functions = [func], mainIndex = 0}
                let result = roundTrip program
                code (head (functions result)) `shouldBe` [AddFloat, SubFloat, MulFloat, DivFloat]

            it "round-trips float comparison instructions" $ do
                let func = CompiledFunction {
                    funcName = "test",
                    paramCount = 0,
                    localVarCount = 0,
                    code = [EqFloat, NeqFloat, LtFloat, GtFloat, LeFloat, GeFloat]
                }
                let program = IRProgram {functions = [func], mainIndex = 0}
                let result = roundTrip program
                code (head (functions result)) `shouldBe` [EqFloat, NeqFloat, LtFloat, GtFloat, LeFloat, GeFloat]

            it "round-trips unary operation instructions" $ do
                let func = CompiledFunction {
                    funcName = "test",
                    paramCount = 0,
                    localVarCount = 0,
                    code = [NegInt, NegFloat, NotBool]
                }
                let program = IRProgram {functions = [func], mainIndex = 0}
                let result = roundTrip program
                code (head (functions result)) `shouldBe` [NegInt, NegFloat, NotBool]

        describe "CompiledFunction" $ do
            it "round-trips function name" $ do
                let func = CompiledFunction {
                    funcName = "myFunction",
                    paramCount = 0,
                    localVarCount = 0,
                    code = []
                }
                let program = IRProgram {functions = [func], mainIndex = 0}
                let result = roundTrip program
                funcName (head (functions result)) `shouldBe` "myFunction"

            it "round-trips param count" $ do
                let func = CompiledFunction {
                    funcName = "test",
                    paramCount = 5,
                    localVarCount = 0,
                    code = []
                }
                let program = IRProgram {functions = [func], mainIndex = 0}
                let result = roundTrip program
                paramCount (head (functions result)) `shouldBe` 5

            it "round-trips local var count" $ do
                let func = CompiledFunction {
                    funcName = "test",
                    paramCount = 0,
                    localVarCount = 10,
                    code = []
                }
                let program = IRProgram {functions = [func], mainIndex = 0}
                let result = roundTrip program
                localVarCount (head (functions result)) `shouldBe` 10

        describe "IRProgram" $ do
            it "round-trips mainIndex" $ do
                let func1 = CompiledFunction {
                    funcName = "helper",
                    paramCount = 0,
                    localVarCount = 0,
                    code = []
                }
                let func2 = CompiledFunction {
                    funcName = "main",
                    paramCount = 0,
                    localVarCount = 0,
                    code = []
                }
                let program = IRProgram {functions = [func1, func2], mainIndex = 1}
                let result = roundTrip program
                mainIndex result `shouldBe` 1

            it "round-trips multiple functions" $ do
                let func1 = CompiledFunction {
                    funcName = "func1",
                    paramCount = 1,
                    localVarCount = 2,
                    code = [GetLocal 0, PushInt 1, AddInt, Return]
                }
                let func2 = CompiledFunction {
                    funcName = "func2",
                    paramCount = 2,
                    localVarCount = 3,
                    code = [GetLocal 0, GetLocal 1, MulInt, Return]
                }
                let func3 = CompiledFunction {
                    funcName = "main",
                    paramCount = 0,
                    localVarCount = 0,
                    code = [PushInt 5, Call 0, PushInt 3, Call 1, Halt]
                }
                let program = IRProgram {functions = [func1, func2, func3], mainIndex = 2}
                let result = roundTrip program
                length (functions result) `shouldBe` 3
                funcName (functions result !! 0) `shouldBe` "func1"
                funcName (functions result !! 1) `shouldBe` "func2"
                funcName (functions result !! 2) `shouldBe` "main"

-- ============================================================================
-- Complete Program Tests
-- ============================================================================

    describe "Complete Program Tests" $ do

        it "round-trips a simple addition program" $ do
            let func = CompiledFunction {
                funcName = "main",
                paramCount = 0,
                localVarCount = 0,
                code = [PushInt 5, PushInt 3, AddInt, Halt]
            }
            let program = IRProgram {functions = [func], mainIndex = 0}
            let result = roundTrip program
            code (head (functions result)) `shouldBe` [PushInt 5, PushInt 3, AddInt, Halt]

        it "round-trips a program with conditionals" $ do
            let func = CompiledFunction {
                funcName = "main",
                paramCount = 0,
                localVarCount = 0,
                code = [PushBool True, JumpIfFalse 4, PushInt 42, Jump 5, PushInt 999, Halt]
            }
            let program = IRProgram {functions = [func], mainIndex = 0}
            let result = roundTrip program
            code (head (functions result)) `shouldBe` [PushBool True, JumpIfFalse 4, PushInt 42, Jump 5, PushInt 999, Halt]

        it "round-trips a program with function calls" $ do
            let addFunc = CompiledFunction {
                funcName = "add",
                paramCount = 2,
                localVarCount = 2,
                code = [GetLocal 0, GetLocal 1, AddInt, Return]
            }
            let mainFunc = CompiledFunction {
                funcName = "main",
                paramCount = 0,
                localVarCount = 0,
                code = [PushInt 10, PushInt 20, Call 0, Halt]
            }
            let program = IRProgram {functions = [addFunc, mainFunc], mainIndex = 1}
            let result = roundTrip program
            funcName (functions result !! 0) `shouldBe` "add"
            funcName (functions result !! 1) `shouldBe` "main"
            code (functions result !! 0) `shouldBe` [GetLocal 0, GetLocal 1, AddInt, Return]
            code (functions result !! 1) `shouldBe` [PushInt 10, PushInt 20, Call 0, Halt]

        it "round-trips a program with float operations" $ do
            let func = CompiledFunction {
                funcName = "main",
                paramCount = 0,
                localVarCount = 0,
                code = [PushFloat 2.5, PushFloat 3.0, AddFloat, Halt]
            }
            let program = IRProgram {functions = [func], mainIndex = 0}
            let result = roundTrip program
            code (head (functions result)) `shouldBe` [PushFloat 2.5, PushFloat 3.0, AddFloat, Halt]

        it "round-trips a program with unary operations" $ do
            let func = CompiledFunction {
                funcName = "main",
                paramCount = 0,
                localVarCount = 0,
                code = [PushInt 5, NegInt, PushFloat 3.14, NegFloat, PushBool True, NotBool, Halt]
            }
            let program = IRProgram {functions = [func], mainIndex = 0}
            let result = roundTrip program
            code (head (functions result)) `shouldBe` [PushInt 5, NegInt, PushFloat 3.14, NegFloat, PushBool True, NotBool, Halt]

-- ============================================================================
-- Value Serialization Tests
-- ============================================================================

    describe "Value Serialization" $ do

        it "round-trips VFloat values" $ do
            let floatBytes = encode (VFloat 3.14159 :: Value)
            let result = decode floatBytes :: Value
            result `shouldBe` VFloat 3.14159

        it "round-trips VInt values" $ do
            let intBytes = encode (VInt 42 :: Value)
            let result = decode intBytes :: Value
            result `shouldBe` VInt 42

        it "round-trips VBool values" $ do
            let boolBytes = encode (VBool True :: Value)
            let result = decode boolBytes :: Value
            result `shouldBe` VBool True

-- ============================================================================
-- File I/O Tests
-- ============================================================================

    describe "File I/O" $ do

        it "saves and loads program from file" $ do
            let func = CompiledFunction {
                funcName = "test",
                paramCount = 0,
                localVarCount = 0,
                code = [PushInt 99, Halt]
            }
            let program = IRProgram {functions = [func], mainIndex = 0}
            let testFile = "/tmp/test_bytecode.gbc"
            saveResult <- saveProgramToFile testFile program
            loadResult <- loadProgramFromFile testFile
            case (saveResult, loadResult) of
                (Right (), Right loadedProgram) -> do
                    funcName (head (functions loadedProgram)) `shouldBe` "test"
                    code (head (functions loadedProgram)) `shouldBe` [PushInt 99, Halt]
                (Left err, _) -> error $ "Failed to save: " ++ err
                (_, Left err) -> error $ "Failed to load: " ++ err
