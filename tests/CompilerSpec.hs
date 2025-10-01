module CompilerSpec (spec) where

import Test.Hspec
import Compiler.Compiler
import AST.AST
import qualified Language.Wasm.Structure as Wasm
import qualified Data.Text.Lazy as T
import Text.Megaparsec.Pos (initialPos, SourcePos)
import qualified Data.Map as Map

-- Helper to create Located values for tests (using dummy position)
loc :: a -> Located a
loc = Located (initialPos "<test>")

-- Empty VarTable for tests that don't use variables
emptyVarTable :: VarTable
emptyVarTable = Map.empty

spec :: Spec
spec = do

-- ============================================================================
-- makeFuncType Tests
-- ============================================================================

    describe "makeFuncType" $ do
        it "converts a function with two int params returning int" $ do
            let func = Function
                    { fType = TypeInt
                    , fName = "add"
                    , fParams = [Parameter TypeInt "a", Parameter TypeInt "b"]
                    , fBody = []
                    }
            let result = makeFuncType func
            result `shouldBe` Wasm.FuncType [Wasm.I64, Wasm.I64] [Wasm.I64]

        it "converts a void function with no params" $ do
            let func = Function
                    { fType = TypeVoid
                    , fName = "doNothing"
                    , fParams = []
                    , fBody = []
                    }
            let result = makeFuncType func
            result `shouldBe` Wasm.FuncType [] []

-- ============================================================================
-- makeExport Tests
-- ============================================================================

    describe "makeExport" $ do
        it "exports add function at index 0" $ do
            let func = Function
                    { fType = TypeInt
                    , fName = "add"
                    , fParams = [Parameter TypeInt "a", Parameter TypeInt "b"]
                    , fBody = []
                    }
            let result = makeExport 0 func
            result `shouldBe` Wasm.Export (T.pack "add") (Wasm.ExportFunc 0)

        it "exports multiply function at index 1" $ do
            let func = Function
                    { fType = TypeInt
                    , fName = "multiply"
                    , fParams = []
                    , fBody = []
                    }
            let result = makeExport 1 func
            result `shouldBe` Wasm.Export (T.pack "multiply") (Wasm.ExportFunc 1)

-- ============================================================================
-- compileFunc Tests
-- ============================================================================

    describe "compileFunc" $ do
        it "compiles a function returning 5 + 3" $ do
            let func = Function
                    { fType = TypeInt
                    , fName = "test"
                    , fParams = []
                    , fBody = [Return (BinOp Add (loc (NumLit (loc 5))) (loc (NumLit (loc 3))))]
                    }
            let Wasm.Function _ _ resultBody = compileFunc 0 func
            resultBody `shouldBe` [Wasm.I64Const 5, Wasm.I64Const 3, Wasm.IBinOp Wasm.BS64 Wasm.IAdd, Wasm.Return]

-- ============================================================================
-- compileExpr Tests
-- ============================================================================

    describe "compileExpr - Literals" $ do
        it "compiles integer literal" $ do
            let result = compileExpr emptyVarTable (NumLit (loc 42))
            result `shouldBe` [Wasm.I64Const 42]

        it "compiles negative integer literal" $ do
            let result = compileExpr emptyVarTable (NumLit (loc (-17)))
            result `shouldBe` [Wasm.I64Const (-17)]

        it "compiles boolean literal true" $ do
            let result = compileExpr emptyVarTable (BoolLit (loc True))
            result `shouldBe` [Wasm.I32Const 1]

        it "compiles boolean literal false" $ do
            let result = compileExpr emptyVarTable (BoolLit (loc False))
            result `shouldBe` [Wasm.I32Const 0]


    describe "compileExpr - Arithmetic Operations" $ do
        it "compiles addition" $ do
            let result = compileExpr emptyVarTable (BinOp Add (loc (NumLit (loc 10))) (loc (NumLit (loc 20))))
            result `shouldBe` [Wasm.I64Const 10, Wasm.I64Const 20, Wasm.IBinOp Wasm.BS64 Wasm.IAdd]

        it "compiles subtraction" $ do
            let result = compileExpr emptyVarTable (BinOp Sub (loc (NumLit (loc 50))) (loc (NumLit (loc 30))))
            result `shouldBe` [Wasm.I64Const 50, Wasm.I64Const 30, Wasm.IBinOp Wasm.BS64 Wasm.ISub]

        it "compiles multiplication" $ do
            let result = compileExpr emptyVarTable (BinOp Mul (loc (NumLit (loc 7))) (loc (NumLit (loc 6))))
            result `shouldBe` [Wasm.I64Const 7, Wasm.I64Const 6, Wasm.IBinOp Wasm.BS64 Wasm.IMul]

        it "compiles division" $ do
            let result = compileExpr emptyVarTable (BinOp Div (loc (NumLit (loc 100))) (loc (NumLit (loc 5))))
            result `shouldBe` [Wasm.I64Const 100, Wasm.I64Const 5, Wasm.IBinOp Wasm.BS64 Wasm.IDivS]


    describe "compileExpr - Comparison Operations" $ do
        it "compiles equality comparison" $ do
            let result = compileExpr emptyVarTable (BinOp Eq (loc (NumLit (loc 5))) (loc (NumLit (loc 5))))
            result `shouldBe` [Wasm.I64Const 5, Wasm.I64Const 5, Wasm.IRelOp Wasm.BS64 Wasm.IEq]

        it "compiles not equal comparison" $ do
            let result = compileExpr emptyVarTable (BinOp Neq (loc (NumLit (loc 3))) (loc (NumLit (loc 7))))
            result `shouldBe` [Wasm.I64Const 3, Wasm.I64Const 7, Wasm.IRelOp Wasm.BS64 Wasm.INe]

        it "compiles less than comparison" $ do
            let result = compileExpr emptyVarTable (BinOp Lt (loc (NumLit (loc 2))) (loc (NumLit (loc 8))))
            result `shouldBe` [Wasm.I64Const 2, Wasm.I64Const 8, Wasm.IRelOp Wasm.BS64 Wasm.ILtS]

        it "compiles greater than comparison" $ do
            let result = compileExpr emptyVarTable (BinOp Gt (loc (NumLit (loc 15))) (loc (NumLit (loc 10))))
            result `shouldBe` [Wasm.I64Const 15, Wasm.I64Const 10, Wasm.IRelOp Wasm.BS64 Wasm.IGtS]

        it "compiles less than or equal comparison" $ do
            let result = compileExpr emptyVarTable (BinOp Le (loc (NumLit (loc 5))) (loc (NumLit (loc 5))))
            result `shouldBe` [Wasm.I64Const 5, Wasm.I64Const 5, Wasm.IRelOp Wasm.BS64 Wasm.ILeS]

        it "compiles greater than or equal comparison" $ do
            let result = compileExpr emptyVarTable (BinOp Ge (loc (NumLit (loc 12))) (loc (NumLit (loc 8))))
            result `shouldBe` [Wasm.I64Const 12, Wasm.I64Const 8, Wasm.IRelOp Wasm.BS64 Wasm.IGeS]


    describe "compileExpr - Logic Operations" $ do
        it "compiles logical AND with two true values" $ do
            let result = compileExpr emptyVarTable (BinOp And (loc (BoolLit (loc True))) (loc (BoolLit (loc True))))
            result `shouldBe` [Wasm.I32Const 1, Wasm.I32Const 1, Wasm.IBinOp Wasm.BS32 Wasm.IAnd]

        it "compiles logical AND with true and false" $ do
            let result = compileExpr emptyVarTable (BinOp And (loc (BoolLit (loc True))) (loc (BoolLit (loc False))))
            result `shouldBe` [Wasm.I32Const 1, Wasm.I32Const 0, Wasm.IBinOp Wasm.BS32 Wasm.IAnd]

        it "compiles logical OR with two false values" $ do
            let result = compileExpr emptyVarTable (BinOp Or (loc (BoolLit (loc False))) (loc (BoolLit (loc False))))
            result `shouldBe` [Wasm.I32Const 0, Wasm.I32Const 0, Wasm.IBinOp Wasm.BS32 Wasm.IOr]

        it "compiles logical OR with false and true" $ do
            let result = compileExpr emptyVarTable (BinOp Or (loc (BoolLit (loc False))) (loc (BoolLit (loc True))))
            result `shouldBe` [Wasm.I32Const 0, Wasm.I32Const 1, Wasm.IBinOp Wasm.BS32 Wasm.IOr]


    describe "compileExpr - Nested Expressions" $ do
        it "compiles nested arithmetic (2 + 3) * 4" $ do
            let expr = BinOp Mul
                        (loc (BinOp Add (loc (NumLit (loc 2))) (loc (NumLit (loc 3)))))
                        (loc (NumLit (loc 4)))
            let result = compileExpr emptyVarTable expr
            result `shouldBe` [ Wasm.I64Const 2, Wasm.I64Const 3, Wasm.IBinOp Wasm.BS64 Wasm.IAdd
                              , Wasm.I64Const 4, Wasm.IBinOp Wasm.BS64 Wasm.IMul ]

        it "compiles comparison of arithmetic expressions (10 + 5) < (20 - 3)" $ do
            let expr = BinOp Lt
                        (loc (BinOp Add (loc (NumLit (loc 10))) (loc (NumLit (loc 5)))))
                        (loc (BinOp Sub (loc (NumLit (loc 20))) (loc (NumLit (loc 3)))))
            let result = compileExpr emptyVarTable expr
            result `shouldBe` [ Wasm.I64Const 10, Wasm.I64Const 5, Wasm.IBinOp Wasm.BS64 Wasm.IAdd
                              , Wasm.I64Const 20, Wasm.I64Const 3, Wasm.IBinOp Wasm.BS64 Wasm.ISub
                              , Wasm.IRelOp Wasm.BS64 Wasm.ILtS ]


-- ============================================================================
-- Variable Access Tests
-- ============================================================================

    describe "compileExpr - Variable Access" $ do
        it "compiles variable access for first parameter" $ do
            let varTable = Map.fromList [("x", 0)]
            let result = compileExpr varTable (Var (loc "x"))
            result `shouldBe` [Wasm.GetLocal 0]

        it "compiles variable access for second parameter" $ do
            let varTable = Map.fromList [("x", 0), ("y", 1)]
            let result = compileExpr varTable (Var (loc "y"))
            result `shouldBe` [Wasm.GetLocal 1]

        it "compiles expression using variables (x + y)" $ do
            let varTable = Map.fromList [("x", 0), ("y", 1)]
            let result = compileExpr varTable (BinOp Add (loc (Var (loc "x"))) (loc (Var (loc "y"))))
            result `shouldBe` [Wasm.GetLocal 0, Wasm.GetLocal 1, Wasm.IBinOp Wasm.BS64 Wasm.IAdd]


-- ============================================================================
-- buildVarTable Tests
-- ============================================================================

    describe "buildVarTable" $ do
        it "builds empty table from no parameters" $ do
            let result = buildVarTable []
            result `shouldBe` Map.empty

        it "builds table from single parameter" $ do
            let result = buildVarTable [Parameter TypeInt "x"]
            result `shouldBe` Map.fromList [("x", 0)]

        it "builds table from multiple parameters" $ do
            let result = buildVarTable [Parameter TypeInt "a", Parameter TypeInt "b", Parameter TypeInt "c"]
            result `shouldBe` Map.fromList [("a", 0), ("b", 1), ("c", 2)]