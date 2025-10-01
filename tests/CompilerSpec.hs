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

-- Empty FuncTable for tests that don't use functions
emptyFuncTable :: FuncTable
emptyFuncTable = Map.empty

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
            let Wasm.Function _ _ resultBody = compileFunc emptyFuncTable 0 func
            resultBody `shouldBe` [Wasm.I64Const 5, Wasm.I64Const 3, Wasm.IBinOp Wasm.BS64 Wasm.IAdd, Wasm.Return]

-- ============================================================================
-- compileExpr Tests
-- ============================================================================

    describe "compileExpr - Literals" $ do
        it "compiles integer literal" $ do
            let result = compileExpr emptyFuncTable emptyVarTable (NumLit (loc 42))
            result `shouldBe` [Wasm.I64Const 42]

        it "compiles negative integer literal" $ do
            let result = compileExpr emptyFuncTable emptyVarTable (NumLit (loc (-17)))
            result `shouldBe` [Wasm.I64Const (-17)]

        it "compiles boolean literal true" $ do
            let result = compileExpr emptyFuncTable emptyVarTable (BoolLit (loc True))
            result `shouldBe` [Wasm.I32Const 1]

        it "compiles boolean literal false" $ do
            let result = compileExpr emptyFuncTable emptyVarTable (BoolLit (loc False))
            result `shouldBe` [Wasm.I32Const 0]


    describe "compileExpr - Arithmetic Operations" $ do
        it "compiles addition" $ do
            let result = compileExpr emptyFuncTable emptyVarTable (BinOp Add (loc (NumLit (loc 10))) (loc (NumLit (loc 20))))
            result `shouldBe` [Wasm.I64Const 10, Wasm.I64Const 20, Wasm.IBinOp Wasm.BS64 Wasm.IAdd]

        it "compiles subtraction" $ do
            let result = compileExpr emptyFuncTable emptyVarTable (BinOp Sub (loc (NumLit (loc 50))) (loc (NumLit (loc 30))))
            result `shouldBe` [Wasm.I64Const 50, Wasm.I64Const 30, Wasm.IBinOp Wasm.BS64 Wasm.ISub]

        it "compiles multiplication" $ do
            let result = compileExpr emptyFuncTable emptyVarTable (BinOp Mul (loc (NumLit (loc 7))) (loc (NumLit (loc 6))))
            result `shouldBe` [Wasm.I64Const 7, Wasm.I64Const 6, Wasm.IBinOp Wasm.BS64 Wasm.IMul]

        it "compiles division" $ do
            let result = compileExpr emptyFuncTable emptyVarTable (BinOp Div (loc (NumLit (loc 100))) (loc (NumLit (loc 5))))
            result `shouldBe` [Wasm.I64Const 100, Wasm.I64Const 5, Wasm.IBinOp Wasm.BS64 Wasm.IDivS]


    describe "compileExpr - Comparison Operations" $ do
        it "compiles equality comparison" $ do
            let result = compileExpr emptyFuncTable emptyVarTable (BinOp Eq (loc (NumLit (loc 5))) (loc (NumLit (loc 5))))
            result `shouldBe` [Wasm.I64Const 5, Wasm.I64Const 5, Wasm.IRelOp Wasm.BS64 Wasm.IEq]

        it "compiles not equal comparison" $ do
            let result = compileExpr emptyFuncTable emptyVarTable (BinOp Neq (loc (NumLit (loc 3))) (loc (NumLit (loc 7))))
            result `shouldBe` [Wasm.I64Const 3, Wasm.I64Const 7, Wasm.IRelOp Wasm.BS64 Wasm.INe]

        it "compiles less than comparison" $ do
            let result = compileExpr emptyFuncTable emptyVarTable (BinOp Lt (loc (NumLit (loc 2))) (loc (NumLit (loc 8))))
            result `shouldBe` [Wasm.I64Const 2, Wasm.I64Const 8, Wasm.IRelOp Wasm.BS64 Wasm.ILtS]

        it "compiles greater than comparison" $ do
            let result = compileExpr emptyFuncTable emptyVarTable (BinOp Gt (loc (NumLit (loc 15))) (loc (NumLit (loc 10))))
            result `shouldBe` [Wasm.I64Const 15, Wasm.I64Const 10, Wasm.IRelOp Wasm.BS64 Wasm.IGtS]

        it "compiles less than or equal comparison" $ do
            let result = compileExpr emptyFuncTable emptyVarTable (BinOp Le (loc (NumLit (loc 5))) (loc (NumLit (loc 5))))
            result `shouldBe` [Wasm.I64Const 5, Wasm.I64Const 5, Wasm.IRelOp Wasm.BS64 Wasm.ILeS]

        it "compiles greater than or equal comparison" $ do
            let result = compileExpr emptyFuncTable emptyVarTable (BinOp Ge (loc (NumLit (loc 12))) (loc (NumLit (loc 8))))
            result `shouldBe` [Wasm.I64Const 12, Wasm.I64Const 8, Wasm.IRelOp Wasm.BS64 Wasm.IGeS]


    describe "compileExpr - Logic Operations" $ do
        it "compiles logical AND with two true values" $ do
            let result = compileExpr emptyFuncTable emptyVarTable (BinOp And (loc (BoolLit (loc True))) (loc (BoolLit (loc True))))
            result `shouldBe` [Wasm.I32Const 1, Wasm.I32Const 1, Wasm.IBinOp Wasm.BS32 Wasm.IAnd]

        it "compiles logical AND with true and false" $ do
            let result = compileExpr emptyFuncTable emptyVarTable (BinOp And (loc (BoolLit (loc True))) (loc (BoolLit (loc False))))
            result `shouldBe` [Wasm.I32Const 1, Wasm.I32Const 0, Wasm.IBinOp Wasm.BS32 Wasm.IAnd]

        it "compiles logical OR with two false values" $ do
            let result = compileExpr emptyFuncTable emptyVarTable (BinOp Or (loc (BoolLit (loc False))) (loc (BoolLit (loc False))))
            result `shouldBe` [Wasm.I32Const 0, Wasm.I32Const 0, Wasm.IBinOp Wasm.BS32 Wasm.IOr]

        it "compiles logical OR with false and true" $ do
            let result = compileExpr emptyFuncTable emptyVarTable (BinOp Or (loc (BoolLit (loc False))) (loc (BoolLit (loc True))))
            result `shouldBe` [Wasm.I32Const 0, Wasm.I32Const 1, Wasm.IBinOp Wasm.BS32 Wasm.IOr]


    describe "compileExpr - Nested Expressions" $ do
        it "compiles nested arithmetic (2 + 3) * 4" $ do
            let expr = BinOp Mul
                        (loc (BinOp Add (loc (NumLit (loc 2))) (loc (NumLit (loc 3)))))
                        (loc (NumLit (loc 4)))
            let result = compileExpr emptyFuncTable emptyVarTable expr
            result `shouldBe` [ Wasm.I64Const 2, Wasm.I64Const 3, Wasm.IBinOp Wasm.BS64 Wasm.IAdd
                              , Wasm.I64Const 4, Wasm.IBinOp Wasm.BS64 Wasm.IMul ]

        it "compiles comparison of arithmetic expressions (10 + 5) < (20 - 3)" $ do
            let expr = BinOp Lt
                        (loc (BinOp Add (loc (NumLit (loc 10))) (loc (NumLit (loc 5)))))
                        (loc (BinOp Sub (loc (NumLit (loc 20))) (loc (NumLit (loc 3)))))
            let result = compileExpr emptyFuncTable emptyVarTable expr
            result `shouldBe` [ Wasm.I64Const 10, Wasm.I64Const 5, Wasm.IBinOp Wasm.BS64 Wasm.IAdd
                              , Wasm.I64Const 20, Wasm.I64Const 3, Wasm.IBinOp Wasm.BS64 Wasm.ISub
                              , Wasm.IRelOp Wasm.BS64 Wasm.ILtS ]


-- ============================================================================
-- Variable Access Tests
-- ============================================================================

    describe "compileExpr - Variable Access" $ do
        it "compiles variable access for first parameter" $ do
            let varTable = Map.fromList [("x", 0)]
            let result = compileExpr emptyFuncTable varTable (Var (loc "x"))
            result `shouldBe` [Wasm.GetLocal 0]

        it "compiles variable access for second parameter" $ do
            let varTable = Map.fromList [("x", 0), ("y", 1)]
            let result = compileExpr emptyFuncTable varTable (Var (loc "y"))
            result `shouldBe` [Wasm.GetLocal 1]

        it "compiles expression using variables (x + y)" $ do
            let varTable = Map.fromList [("x", 0), ("y", 1)]
            let result = compileExpr emptyFuncTable varTable (BinOp Add (loc (Var (loc "x"))) (loc (Var (loc "y"))))
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


-- ============================================================================
-- buildFuncTable Tests
-- ============================================================================

    describe "buildFuncTable" $ do
        it "builds empty table from no functions" $ do
            let result = buildFuncTable []
            result `shouldBe` Map.empty

        it "builds table from single function" $ do
            let func = Function TypeInt "add" [] []
            let result = buildFuncTable [func]
            result `shouldBe` Map.fromList [("add", 0)]

        it "builds table from multiple functions" $ do
            let func1 = Function TypeInt "main" [] []
            let func2 = Function TypeInt "add" [] []
            let func3 = Function TypeVoid "helper" [] []
            let result = buildFuncTable [func1, func2, func3]
            result `shouldBe` Map.fromList [("main", 0), ("add", 1), ("helper", 2)]


-- ============================================================================
-- Function Call Tests
-- ============================================================================

    describe "compileExpr - Function Calls" $ do
        it "compiles function call with no arguments" $ do
            let funcTable = Map.fromList [("getAnswer", 0)]
            let result = compileExpr funcTable emptyVarTable (Call (loc "getAnswer") [])
            result `shouldBe` [Wasm.Call 0]

        it "compiles function call with one argument" $ do
            let funcTable = Map.fromList [("double", 1)]
            let result = compileExpr funcTable emptyVarTable (Call (loc "double") [NumLit (loc 21)])
            result `shouldBe` [Wasm.I64Const 21, Wasm.Call 1]

        it "compiles function call with multiple arguments" $ do
            let funcTable = Map.fromList [("add", 2)]
            let result = compileExpr funcTable emptyVarTable (Call (loc "add") [NumLit (loc 10), NumLit (loc 20)])
            result `shouldBe` [Wasm.I64Const 10, Wasm.I64Const 20, Wasm.Call 2]

        it "compiles function call with expression arguments" $ do
            let funcTable = Map.fromList [("multiply", 0)]
            let arg1 = BinOp Add (loc (NumLit (loc 2))) (loc (NumLit (loc 3)))
            let arg2 = NumLit (loc 4)
            let result = compileExpr funcTable emptyVarTable (Call (loc "multiply") [arg1, arg2])
            result `shouldBe` [Wasm.I64Const 2, Wasm.I64Const 3, Wasm.IBinOp Wasm.BS64 Wasm.IAdd, Wasm.I64Const 4, Wasm.Call 0]

        it "compiles function call with variable arguments" $ do
            let funcTable = Map.fromList [("sum", 3)]
            let varTable = Map.fromList [("x", 0), ("y", 1)]
            let result = compileExpr funcTable varTable (Call (loc "sum") [Var (loc "x"), Var (loc "y")])
            result `shouldBe` [Wasm.GetLocal 0, Wasm.GetLocal 1, Wasm.Call 3]

        it "compiles nested function calls" $ do
            let funcTable = Map.fromList [("inner", 0), ("outer", 1)]
            let innerCall = Call (loc "inner") [NumLit (loc 5)]
            let result = compileExpr funcTable emptyVarTable (Call (loc "outer") [innerCall])
            result `shouldBe` [Wasm.I64Const 5, Wasm.Call 0, Wasm.Call 1]


-- ============================================================================
-- collectDecls Tests
-- ============================================================================

    describe "collectDecls" $ do
        it "collects no declarations from empty list" $ do
            let result = collectDecls []
            result `shouldBe` []

        it "collects single declaration" $ do
            let result = collectDecls [Decl TypeInt "x" Nothing]
            result `shouldBe` [("x", TypeInt)]

        it "collects multiple declarations" $ do
            let stmts = [Decl TypeInt "x" Nothing, Decl TypeBool "y" Nothing, Decl TypeInt "z" Nothing]
            let result = collectDecls stmts
            result `shouldBe` [("x", TypeInt), ("y", TypeBool), ("z", TypeInt)]

        it "collects declarations from nested if statements" $ do
            let stmts = [If (BoolLit (loc True)) [Decl TypeInt "a" Nothing] Nothing]
            let result = collectDecls stmts
            result `shouldBe` [("a", TypeInt)]

        it "collects declarations from nested while loops" $ do
            let stmts = [While (BoolLit (loc True)) [Decl TypeInt "counter" Nothing]]
            let result = collectDecls stmts
            result `shouldBe` [("counter", TypeInt)]

        it "ignores non-declaration statements" $ do
            let stmts = [Return (NumLit (loc 42)), Assign "x" (NumLit (loc 5))]
            let result = collectDecls stmts
            result `shouldBe` []


-- ============================================================================
-- Statement Compilation Tests
-- ============================================================================

    describe "compileStatement - Decl" $ do
        it "compiles declaration without initializer" $ do
            let varTable = Map.fromList [("x", 0)]
            let result = compileStatement emptyFuncTable varTable (Decl TypeInt "x" Nothing)
            result `shouldBe` []

        it "compiles declaration with literal initializer" $ do
            let varTable = Map.fromList [("x", 0)]
            let result = compileStatement emptyFuncTable varTable (Decl TypeInt "x" (Just (NumLit (loc 42))))
            result `shouldBe` [Wasm.I64Const 42, Wasm.SetLocal 0]

        it "compiles declaration with expression initializer" $ do
            let varTable = Map.fromList [("x", 0)]
            let initExpr = BinOp Add (loc (NumLit (loc 10))) (loc (NumLit (loc 20)))
            let result = compileStatement emptyFuncTable varTable (Decl TypeInt "x" (Just initExpr))
            result `shouldBe` [Wasm.I64Const 10, Wasm.I64Const 20, Wasm.IBinOp Wasm.BS64 Wasm.IAdd, Wasm.SetLocal 0]


    describe "compileStatement - Assign" $ do
        it "compiles assignment with literal" $ do
            let varTable = Map.fromList [("x", 0)]
            let result = compileStatement emptyFuncTable varTable (Assign "x" (NumLit (loc 100)))
            result `shouldBe` [Wasm.I64Const 100, Wasm.SetLocal 0]

        it "compiles assignment with expression" $ do
            let varTable = Map.fromList [("x", 0), ("y", 1)]
            let expr = BinOp Mul (loc (Var (loc "y"))) (loc (NumLit (loc 2)))
            let result = compileStatement emptyFuncTable varTable (Assign "x" expr)
            result `shouldBe` [Wasm.GetLocal 1, Wasm.I64Const 2, Wasm.IBinOp Wasm.BS64 Wasm.IMul, Wasm.SetLocal 0]


    describe "compileStatement - ExprStmt" $ do
        it "compiles expression statement and drops result" $ do
            let funcTable = Map.fromList [("print", 0)]
            let result = compileStatement funcTable emptyVarTable (ExprStmt (Call (loc "print") [NumLit (loc 42)]))
            result `shouldBe` [Wasm.I64Const 42, Wasm.Call 0, Wasm.Drop]


    describe "compileStatement - If" $ do
        it "compiles if statement without else" $ do
            let varTable = Map.fromList [("x", 0)]
            let ifStmt = If (BoolLit (loc True)) [Assign "x" (NumLit (loc 1))] Nothing
            let result = compileStatement emptyFuncTable varTable ifStmt
            result `shouldBe` [Wasm.I32Const 1, Wasm.If (Wasm.Inline Nothing) [Wasm.I64Const 1, Wasm.SetLocal 0] []]

        it "compiles if statement with else" $ do
            let varTable = Map.fromList [("x", 0)]
            let ifStmt = If (BoolLit (loc False)) [Assign "x" (NumLit (loc 1))] (Just [Assign "x" (NumLit (loc 2))])
            let result = compileStatement emptyFuncTable varTable ifStmt
            result `shouldBe` [Wasm.I32Const 0, Wasm.If (Wasm.Inline Nothing) [Wasm.I64Const 1, Wasm.SetLocal 0] [Wasm.I64Const 2, Wasm.SetLocal 0]]


    describe "compileStatement - While" $ do
        it "compiles while loop" $ do
            let varTable = Map.fromList [("i", 0)]
            let condition = BinOp Lt (loc (Var (loc "i"))) (loc (NumLit (loc 10)))
            let body = [Assign "i" (BinOp Add (loc (Var (loc "i"))) (loc (NumLit (loc 1))))]
            let whileStmt = While condition body
            let result = compileStatement emptyFuncTable varTable whileStmt
            let expectedLoop = [Wasm.GetLocal 0, Wasm.I64Const 10, Wasm.IRelOp Wasm.BS64 Wasm.ILtS,
                               Wasm.If (Wasm.Inline Nothing)
                                 [Wasm.GetLocal 0, Wasm.I64Const 1, Wasm.IBinOp Wasm.BS64 Wasm.IAdd, Wasm.SetLocal 0, Wasm.Br 1]
                                 [Wasm.Br 0]]
            result `shouldBe` [Wasm.Block (Wasm.Inline Nothing) [Wasm.Loop (Wasm.Inline Nothing) expectedLoop]]


-- ============================================================================
-- Integration Tests with compileFunc
-- ============================================================================

    describe "compileFunc with local variables" $ do
        it "compiles function with local variable declaration" $ do
            let func = Function
                    { fType = TypeInt
                    , fName = "test"
                    , fParams = []
                    , fBody = [ Decl TypeInt "x" (Just (NumLit (loc 42)))
                              , Return (Var (loc "x"))
                              ]
                    }
            let Wasm.Function _ localTypes resultBody = compileFunc emptyFuncTable 0 func
            localTypes `shouldBe` [Wasm.I64]
            resultBody `shouldBe` [Wasm.I64Const 42, Wasm.SetLocal 0, Wasm.GetLocal 0, Wasm.Return]

        it "compiles function with parameters and locals" $ do
            let func = Function
                    { fType = TypeInt
                    , fName = "add_and_square"
                    , fParams = [Parameter TypeInt "a", Parameter TypeInt "b"]
                    , fBody = [ Decl TypeInt "sum" (Just (BinOp Add (loc (Var (loc "a"))) (loc (Var (loc "b")))))
                              , Return (BinOp Mul (loc (Var (loc "sum"))) (loc (Var (loc "sum"))))
                              ]
                    }
            let Wasm.Function _ localTypes _ = compileFunc emptyFuncTable 0 func
            localTypes `shouldBe` [Wasm.I64]


-- ============================================================================
-- astToWasm Integration Tests - The Real Deal
-- ============================================================================

    describe "astToWasm - Edge Cases & Full Integration" $ do

        it "compiles empty program (no functions)" $ do
            let program = Program []
            let result = astToWasm program
            Wasm.types result `shouldBe` []
            Wasm.functions result `shouldBe` []
            Wasm.exports result `shouldBe` []
            Wasm.tables result `shouldBe` []
            Wasm.mems result `shouldBe` []
            Wasm.globals result `shouldBe` []

        it "compiles single function returning constant" $ do
            let func = Function
                    { fType = TypeInt
                    , fName = "fortytwo"
                    , fParams = []
                    , fBody = [Return (NumLit (loc 42))]
                    }
            let program = Program [func]
            let result = astToWasm program
            length (Wasm.types result) `shouldBe` 1
            length (Wasm.functions result) `shouldBe` 1
            length (Wasm.exports result) `shouldBe` 1
            Wasm.types result `shouldBe` [Wasm.FuncType [] [Wasm.I64]]
            Wasm.exports result `shouldBe` [Wasm.Export (T.pack "fortytwo") (Wasm.ExportFunc 0)]

        it "compiles void function" $ do
            let func = Function
                    { fType = TypeVoid
                    , fName = "doNothing"
                    , fParams = []
                    , fBody = []
                    }
            let program = Program [func]
            let result = astToWasm program
            Wasm.types result `shouldBe` [Wasm.FuncType [] []]
            length (Wasm.functions result) `shouldBe` 1

        it "compiles multiple functions with correct indices" $ do
            let func1 = Function TypeInt "first" [] [Return (NumLit (loc 1))]
            let func2 = Function TypeInt "second" [] [Return (NumLit (loc 2))]
            let func3 = Function TypeInt "third" [] [Return (NumLit (loc 3))]
            let program = Program [func1, func2, func3]
            let result = astToWasm program
            length (Wasm.types result) `shouldBe` 3
            length (Wasm.functions result) `shouldBe` 3
            length (Wasm.exports result) `shouldBe` 3
            Wasm.exports result `shouldBe` [ Wasm.Export (T.pack "first") (Wasm.ExportFunc 0)
                                           , Wasm.Export (T.pack "second") (Wasm.ExportFunc 1)
                                           , Wasm.Export (T.pack "third") (Wasm.ExportFunc 2)
                                           ]

        it "compiles function with parameters" $ do
            let func = Function
                    { fType = TypeInt
                    , fName = "add"
                    , fParams = [Parameter TypeInt "a", Parameter TypeInt "b"]
                    , fBody = [Return (BinOp Add (loc (Var (loc "a"))) (loc (Var (loc "b"))))]
                    }
            let program = Program [func]
            let result = astToWasm program
            Wasm.types result `shouldBe` [Wasm.FuncType [Wasm.I64, Wasm.I64] [Wasm.I64]]

        it "compiles function calling another function" $ do
            let helper = Function TypeInt "helper" [] [Return (NumLit (loc 42))]
            let main = Function
                    { fType = TypeInt
                    , fName = "main"
                    , fParams = []
                    , fBody = [Return (Call (loc "helper") [])]
                    }
            let program = Program [helper, main]
            let result = astToWasm program
            length (Wasm.functions result) `shouldBe` 2
            let Wasm.Function _ _ mainBody = Wasm.functions result !! 1
            mainBody `shouldBe` [Wasm.Call 0, Wasm.Return]

        it "compiles function with all statement types" $ do
            let func = Function
                    { fType = TypeInt
                    , fName = "complex"
                    , fParams = [Parameter TypeInt "n"]
                    , fBody = [ Decl TypeInt "result" (Just (NumLit (loc 0)))
                              , Decl TypeInt "i" (Just (NumLit (loc 0)))
                              , While (BinOp Lt (loc (Var (loc "i"))) (loc (Var (loc "n"))))
                                  [ Assign "result" (BinOp Add (loc (Var (loc "result"))) (loc (Var (loc "i"))))
                                  , Assign "i" (BinOp Add (loc (Var (loc "i"))) (loc (NumLit (loc 1))))
                                  ]
                              , If (BinOp Gt (loc (Var (loc "result"))) (loc (NumLit (loc 100))))
                                  [Assign "result" (NumLit (loc 100))]
                                  Nothing
                              , Return (Var (loc "result"))
                              ]
                    }
            let program = Program [func]
            let result = astToWasm program
            let Wasm.Function _ localTypes _ = head (Wasm.functions result)
            localTypes `shouldBe` [Wasm.I64, Wasm.I64]

        it "compiles function with nested if-else" $ do
            let func = Function
                    { fType = TypeInt
                    , fName = "nested"
                    , fParams = [Parameter TypeInt "x"]
                    , fBody = [ If (BinOp Gt (loc (Var (loc "x"))) (loc (NumLit (loc 10))))
                                  [ If (BinOp Lt (loc (Var (loc "x"))) (loc (NumLit (loc 20))))
                                      [Return (NumLit (loc 1))]
                                      (Just [Return (NumLit (loc 2))])
                                  ]
                                  (Just [Return (NumLit (loc 3))])
                              , Return (NumLit (loc 0))
                              ]
                    }
            let program = Program [func]
            let result = astToWasm program
            length (Wasm.functions result) `shouldBe` 1

        it "compiles function with multiple local variables" $ do
            let func = Function
                    { fType = TypeInt
                    , fName = "manyLocals"
                    , fParams = []
                    , fBody = [ Decl TypeInt "a" (Just (NumLit (loc 1)))
                              , Decl TypeInt "b" (Just (NumLit (loc 2)))
                              , Decl TypeInt "c" (Just (NumLit (loc 3)))
                              , Decl TypeInt "d" (Just (NumLit (loc 4)))
                              , Decl TypeInt "e" (Just (NumLit (loc 5)))
                              , Return (BinOp Add
                                  (loc (BinOp Add (loc (Var (loc "a"))) (loc (Var (loc "b")))))
                                  (loc (BinOp Add (loc (Var (loc "c"))) (loc (BinOp Add (loc (Var (loc "d"))) (loc (Var (loc "e"))))))))
                              ]
                    }
            let program = Program [func]
            let result = astToWasm program
            let Wasm.Function _ localTypes _ = head (Wasm.functions result)
            localTypes `shouldBe` [Wasm.I64, Wasm.I64, Wasm.I64, Wasm.I64, Wasm.I64]

        it "compiles function with many parameters" $ do
            let func = Function
                    { fType = TypeInt
                    , fName = "sumFive"
                    , fParams = [ Parameter TypeInt "a", Parameter TypeInt "b"
                                , Parameter TypeInt "c", Parameter TypeInt "d"
                                , Parameter TypeInt "e"
                                ]
                    , fBody = [Return (BinOp Add
                                (loc (BinOp Add (loc (Var (loc "a"))) (loc (Var (loc "b")))))
                                (loc (BinOp Add (loc (Var (loc "c"))) (loc (BinOp Add (loc (Var (loc "d"))) (loc (Var (loc "e"))))))))]
                    }
            let program = Program [func]
            let result = astToWasm program
            Wasm.types result `shouldBe` [Wasm.FuncType [Wasm.I64, Wasm.I64, Wasm.I64, Wasm.I64, Wasm.I64] [Wasm.I64]]

        it "compiles recursive function call" $ do
            let func = Function
                    { fType = TypeInt
                    , fName = "factorial"
                    , fParams = [Parameter TypeInt "n"]
                    , fBody = [ If (BinOp Le (loc (Var (loc "n"))) (loc (NumLit (loc 1))))
                                  [Return (NumLit (loc 1))]
                                  (Just [Return (BinOp Mul
                                      (loc (Var (loc "n")))
                                      (loc (Call (loc "factorial") [BinOp Sub (loc (Var (loc "n"))) (loc (NumLit (loc 1)))])))])
                              , Return (NumLit (loc 1))
                              ]
                    }
            let program = Program [func]
            let result = astToWasm program
            length (Wasm.functions result) `shouldBe` 1

        it "compiles function with expression statement (function call for side effects)" $ do
            let helper = Function TypeInt "sideEffect" [] [Return (NumLit (loc 0))]
            let main = Function
                    { fType = TypeInt
                    , fName = "main"
                    , fParams = []
                    , fBody = [ ExprStmt (Call (loc "sideEffect") [])
                              , Return (NumLit (loc 42))
                              ]
                    }
            let program = Program [helper, main]
            let result = astToWasm program
            let Wasm.Function _ _ mainBody = Wasm.functions result !! 1
            -- Should have: Call 0, Drop, I64Const 42, Return
            mainBody `shouldBe` [Wasm.Call 0, Wasm.Drop, Wasm.I64Const 42, Wasm.Return]

        it "compiles function with boolean operations" $ do
            let func = Function
                    { fType = TypeBool
                    , fName = "logicTest"
                    , fParams = [Parameter TypeBool "a", Parameter TypeBool "b"]
                    , fBody = [Return (BinOp And (loc (Var (loc "a"))) (loc (Var (loc "b"))))]
                    }
            let program = Program [func]
            let result = astToWasm program
            Wasm.types result `shouldBe` [Wasm.FuncType [Wasm.I32, Wasm.I32] [Wasm.I32]]

        it "compiles function with mixed bool and int types" $ do
            let func = Function
                    { fType = TypeInt
                    , fName = "conditional"
                    , fParams = [Parameter TypeBool "flag"]
                    , fBody = [ If (Var (loc "flag"))
                                  [Return (NumLit (loc 100))]
                                  (Just [Return (NumLit (loc 200))])
                              , Return (NumLit (loc 0))
                              ]
                    }
            let program = Program [func]
            let result = astToWasm program
            Wasm.types result `shouldBe` [Wasm.FuncType [Wasm.I32] [Wasm.I64]]

        it "compiles program with functions that call each other" $ do
            let func1 = Function
                    { fType = TypeInt
                    , fName = "funcA"
                    , fParams = [Parameter TypeInt "x"]
                    , fBody = [Return (Call (loc "funcB") [Var (loc "x")])]
                    }
            let func2 = Function
                    { fType = TypeInt
                    , fName = "funcB"
                    , fParams = [Parameter TypeInt "y"]
                    , fBody = [Return (BinOp Mul (loc (Var (loc "y"))) (loc (NumLit (loc 2))))]
                    }
            let program = Program [func1, func2]
            let result = astToWasm program
            let Wasm.Function _ _ func1Body = Wasm.functions result !! 0
            -- funcA should call funcB (index 1)
            func1Body `shouldBe` [Wasm.GetLocal 0, Wasm.Call 1, Wasm.Return]

        it "compiles function with locals declared inside if blocks" $ do
            let func = Function
                    { fType = TypeInt
                    , fName = "nestedDecl"
                    , fParams = [Parameter TypeInt "x"]
                    , fBody = [ If (BinOp Gt (loc (Var (loc "x"))) (loc (NumLit (loc 0))))
                                  [Decl TypeInt "positive" (Just (NumLit (loc 1)))]
                                  (Just [Decl TypeInt "negative" (Just (NumLit (loc (-1))))])
                              , Return (NumLit (loc 0))
                              ]
                    }
            let program = Program [func]
            let result = astToWasm program
            let Wasm.Function _ localTypes _ = head (Wasm.functions result)
            -- Should collect both locals
            length localTypes `shouldBe` 2

        it "compiles function with locals declared inside while loops" $ do
            let func = Function
                    { fType = TypeInt
                    , fName = "loopDecl"
                    , fParams = []
                    , fBody = [ While (BoolLit (loc False))
                                  [Decl TypeInt "temp" (Just (NumLit (loc 99)))]
                              , Return (NumLit (loc 0))
                              ]
                    }
            let program = Program [func]
            let result = astToWasm program
            let Wasm.Function _ localTypes _ = head (Wasm.functions result)
            localTypes `shouldBe` [Wasm.I64]

        it "compiles large program with many functions" $ do
            let funcs = [ Function TypeInt ("func" ++ show i) [] [Return (NumLit (loc i))] | i <- [0..9] ]
            let program = Program funcs
            let result = astToWasm program
            length (Wasm.types result) `shouldBe` 10
            length (Wasm.functions result) `shouldBe` 10
            length (Wasm.exports result) `shouldBe` 10
            -- Check exports are correctly named and indexed
            Wasm.exports result !! 0 `shouldBe` Wasm.Export (T.pack "func0") (Wasm.ExportFunc 0)
            Wasm.exports result !! 5 `shouldBe` Wasm.Export (T.pack "func5") (Wasm.ExportFunc 5)
            Wasm.exports result !! 9 `shouldBe` Wasm.Export (T.pack "func9") (Wasm.ExportFunc 9)