module CompilerSpec (spec) where

import Test.Hspec
import Text.Megaparsec.Pos (initialPos)
import qualified Data.Map as M
import AST.AST
import qualified IR.Types as IR
import IR.Types (IRProgram(..), CompiledFunction(..))
import Compiler.Core (compileProgram)
import Compiler.Expr (compileExpr)
import Compiler.Statement (compileStatement, compileStatements)
import Compiler.Function (compileFunction)
import Compiler.Environment
import qualified Security.Environment as SE
import Security.Types (CheckEnv)
import Parser.Core (parseProgram)
import Security.TypeChecker (checkProgram)

-- Helper: to create a Located value at a dummy position
loc :: a -> Located a
loc = Located (initialPos "test")

-- Helper: to create a simple CheckEnv
emptyCheckEnv :: CheckEnv
emptyCheckEnv = SE.makeFunctionEnv M.empty (simpleFunc "dummy" [] [])

-- Helper: to create a CheckEnv with a single variable
checkEnvWithVar :: String -> Type -> CheckEnv
checkEnvWithVar name typ = SE.markInitialized name $ SE.addVar name typ emptyCheckEnv

-- Helper: to create a simple function for testing
simpleFunc :: String -> [Parameter] -> [Statement] -> Function
simpleFunc name params body = Function {
    fType = TypeInt,
    fName = name,
    fParams = params,
    fBody = body
}

spec :: Spec
spec = do

-- ============================================================================
-- Compiler.Environment Tests
-- ============================================================================

    describe "Compiler.Environment" $ do

        describe "makeParamTable" $ do
            it "creates empty table for no parameters" $ do
                let params = []
                let table = makeParamTable params
                M.null table `shouldBe` True

            it "creates table with single parameter" $ do
                let params = [Parameter TypeInt "x"]
                let table = makeParamTable params
                M.lookup "x" table `shouldBe` Just 0

            it "creates table with multiple parameters in order" $ do
                let params = [Parameter TypeInt "a", Parameter TypeFloat "b", Parameter TypeBool "c"]
                let table = makeParamTable params
                M.lookup "a" table `shouldBe` Just 0
                M.lookup "b" table `shouldBe` Just 1
                M.lookup "c" table `shouldBe` Just 2

        describe "makeFuncTable" $ do
            it "creates empty table for no functions" $ do
                let funcs = []
                let table = makeFuncTable funcs
                M.null table `shouldBe` True

            it "creates table with single function" $ do
                let funcs = [simpleFunc "main" [] []]
                let table = makeFuncTable funcs
                M.lookup "main" table `shouldBe` Just 0

            it "creates table with multiple functions in order" $ do
                let funcs = [simpleFunc "foo" [] [], simpleFunc "bar" [] [], simpleFunc "main" [] []]
                let table = makeFuncTable funcs
                M.lookup "foo" table `shouldBe` Just 0
                M.lookup "bar" table `shouldBe` Just 1
                M.lookup "main" table `shouldBe` Just 2

        describe "collectLocalDecls" $ do
            it "returns empty list for no declarations" $ do
                let stmts = []
                collectLocalDecls stmts `shouldBe` []

            it "collects simple declaration" $ do
                let stmts = [Decl TypeInt "x" Nothing]
                collectLocalDecls stmts `shouldBe` [("x", TypeInt)]

            it "collects multiple declarations" $ do
                let stmts = [Decl TypeInt "x" Nothing, Decl TypeFloat "y" Nothing, Decl TypeBool "z" Nothing]
                collectLocalDecls stmts `shouldBe` [("x", TypeInt), ("y", TypeFloat), ("z", TypeBool)]

            it "collects declarations inside if branches" $ do
                let stmts = [If (BoolLit (loc True)) [Decl TypeInt "x" Nothing] (Just [Decl TypeInt "y" Nothing])]
                let result = collectLocalDecls stmts
                result `shouldContain` [("x", TypeInt)]
                result `shouldContain` [("y", TypeInt)]

            it "collects declarations inside while loop" $ do
                let stmts = [While (BoolLit (loc True)) [Decl TypeInt "counter" Nothing]]
                collectLocalDecls stmts `shouldBe` [("counter", TypeInt)]

            it "collects declarations inside for loop (init)" $ do
                let initStmt = Just (Decl TypeInt "i" (Just (NumLit (loc 0))))
                let stmts = [For initStmt Nothing Nothing []]
                collectLocalDecls stmts `shouldBe` [("i", TypeInt)]

            it "collects declarations inside for loop (body)" $ do
                let stmts = [For Nothing Nothing Nothing [Decl TypeInt "temp" Nothing]]
                collectLocalDecls stmts `shouldBe` [("temp", TypeInt)]

            it "collects nested declarations" $ do
                let innerIf = If (BoolLit (loc True)) [Decl TypeInt "inner" Nothing] Nothing
                let outerWhile = While (BoolLit (loc True)) [Decl TypeInt "outer" Nothing, innerIf]
                let result = collectLocalDecls [outerWhile]
                result `shouldContain` [("outer", TypeInt)]
                result `shouldContain` [("inner", TypeInt)]

        describe "buildVarTable" $ do
            it "builds table with only parameters" $ do
                let params = [Parameter TypeInt "x", Parameter TypeInt "y"]
                let body = []
                let table = buildVarTable params body
                M.lookup "x" table `shouldBe` Just 0
                M.lookup "y" table `shouldBe` Just 1

            it "builds table with only local variables" $ do
                let params = []
                let body = [Decl TypeInt "a" Nothing, Decl TypeInt "b" Nothing]
                let table = buildVarTable params body
                M.lookup "a" table `shouldBe` Just 0
                M.lookup "b" table `shouldBe` Just 1

            it "builds table with parameters and locals (params come first)" $ do
                let params = [Parameter TypeInt "x"]
                let body = [Decl TypeInt "a" Nothing, Decl TypeInt "b" Nothing]
                let table = buildVarTable params body
                M.lookup "x" table `shouldBe` Just 0
                M.lookup "a" table `shouldBe` Just 1
                M.lookup "b" table `shouldBe` Just 2

-- ============================================================================
-- Compiler.Expr Tests
-- ============================================================================

    describe "Compiler.Expr" $ do

        describe "Boolean Literals" $ do
            it "compiles true literal" $ do
                let expr = BoolLit (loc True)
                let result = compileExpr emptyCheckEnv M.empty M.empty expr
                result `shouldBe` Right [IR.PushBool True]

            it "compiles false literal" $ do
                let expr = BoolLit (loc False)
                let result = compileExpr emptyCheckEnv M.empty M.empty expr
                result `shouldBe` Right [IR.PushBool False]

        describe "Integer Literals" $ do
            it "compiles positive integer" $ do
                let expr = NumLit (loc 42)
                let result = compileExpr emptyCheckEnv M.empty M.empty expr
                result `shouldBe` Right [IR.PushInt 42]

            it "compiles zero" $ do
                let expr = NumLit (loc 0)
                let result = compileExpr emptyCheckEnv M.empty M.empty expr
                result `shouldBe` Right [IR.PushInt 0]

            it "compiles negative integer" $ do
                let expr = NumLit (loc (-5))
                let result = compileExpr emptyCheckEnv M.empty M.empty expr
                result `shouldBe` Right [IR.PushInt (-5)]

            it "compiles large integer" $ do
                let expr = NumLit (loc 2147483647)
                let result = compileExpr emptyCheckEnv M.empty M.empty expr
                result `shouldBe` Right [IR.PushInt 2147483647]

        describe "Float Literals" $ do
            it "compiles positive float" $ do
                let expr = FloatLit (loc 3.14)
                let result = compileExpr emptyCheckEnv M.empty M.empty expr
                result `shouldBe` Right [IR.PushFloat 3.14]

            it "compiles zero float" $ do
                let expr = FloatLit (loc 0.0)
                let result = compileExpr emptyCheckEnv M.empty M.empty expr
                result `shouldBe` Right [IR.PushFloat 0.0]

            it "compiles negative float" $ do
                let expr = FloatLit (loc (-2.5))
                let result = compileExpr emptyCheckEnv M.empty M.empty expr
                result `shouldBe` Right [IR.PushFloat (-2.5)]

        describe "Variable References" $ do
            it "compiles variable at index 0" $ do
                let expr = Var (loc "x")
                let varTable = M.fromList [("x", 0)]
                let result = compileExpr emptyCheckEnv M.empty varTable expr
                result `shouldBe` Right [IR.GetLocal 0]

            it "compiles variable at higher index" $ do
                let expr = Var (loc "z")
                let varTable = M.fromList [("x", 0), ("y", 1), ("z", 2)]
                let result = compileExpr emptyCheckEnv M.empty varTable expr
                result `shouldBe` Right [IR.GetLocal 2]

        describe "Integer Binary Operations" $ do
            it "compiles integer addition" $ do
                let expr = BinOp Add (loc (NumLit (loc 5))) (loc (NumLit (loc 3)))
                let env = checkEnvWithVar "dummy" TypeInt
                let result = compileExpr env M.empty M.empty expr
                result `shouldBe` Right [IR.PushInt 5, IR.PushInt 3, IR.AddInt]

            it "compiles integer subtraction" $ do
                let expr = BinOp Sub (loc (NumLit (loc 10))) (loc (NumLit (loc 3)))
                let env = checkEnvWithVar "dummy" TypeInt
                let result = compileExpr env M.empty M.empty expr
                result `shouldBe` Right [IR.PushInt 10, IR.PushInt 3, IR.SubInt]

            it "compiles integer multiplication" $ do
                let expr = BinOp Mul (loc (NumLit (loc 4))) (loc (NumLit (loc 7)))
                let env = checkEnvWithVar "dummy" TypeInt
                let result = compileExpr env M.empty M.empty expr
                result `shouldBe` Right [IR.PushInt 4, IR.PushInt 7, IR.MulInt]

            it "compiles integer division" $ do
                let expr = BinOp Div (loc (NumLit (loc 15))) (loc (NumLit (loc 3)))
                let env = checkEnvWithVar "dummy" TypeInt
                let result = compileExpr env M.empty M.empty expr
                result `shouldBe` Right [IR.PushInt 15, IR.PushInt 3, IR.DivInt]

        describe "Float Binary Operations" $ do
            it "compiles float addition" $ do
                let expr = BinOp Add (loc (FloatLit (loc 2.5))) (loc (FloatLit (loc 1.5)))
                let env = checkEnvWithVar "dummy" TypeFloat
                let result = compileExpr env M.empty M.empty expr
                result `shouldBe` Right [IR.PushFloat 2.5, IR.PushFloat 1.5, IR.AddFloat]

            it "compiles float subtraction" $ do
                let expr = BinOp Sub (loc (FloatLit (loc 5.0))) (loc (FloatLit (loc 2.5)))
                let env = checkEnvWithVar "dummy" TypeFloat
                let result = compileExpr env M.empty M.empty expr
                result `shouldBe` Right [IR.PushFloat 5.0, IR.PushFloat 2.5, IR.SubFloat]

            it "compiles float multiplication" $ do
                let expr = BinOp Mul (loc (FloatLit (loc 2.0))) (loc (FloatLit (loc 3.14)))
                let env = checkEnvWithVar "dummy" TypeFloat
                let result = compileExpr env M.empty M.empty expr
                result `shouldBe` Right [IR.PushFloat 2.0, IR.PushFloat 3.14, IR.MulFloat]

            it "compiles float division" $ do
                let expr = BinOp Div (loc (FloatLit (loc 10.0))) (loc (FloatLit (loc 2.0)))
                let env = checkEnvWithVar "dummy" TypeFloat
                let result = compileExpr env M.empty M.empty expr
                result `shouldBe` Right [IR.PushFloat 10.0, IR.PushFloat 2.0, IR.DivFloat]

        describe "Comparison Operations" $ do
            it "compiles integer equality" $ do
                let expr = BinOp Eq (loc (NumLit (loc 5))) (loc (NumLit (loc 5)))
                let env = checkEnvWithVar "dummy" TypeInt
                let result = compileExpr env M.empty M.empty expr
                result `shouldBe` Right [IR.PushInt 5, IR.PushInt 5, IR.EqInt]

            it "compiles integer not-equal" $ do
                let expr = BinOp Neq (loc (NumLit (loc 5))) (loc (NumLit (loc 3)))
                let env = checkEnvWithVar "dummy" TypeInt
                let result = compileExpr env M.empty M.empty expr
                result `shouldBe` Right [IR.PushInt 5, IR.PushInt 3, IR.NeqInt]

            it "compiles integer less-than" $ do
                let expr = BinOp Lt (loc (NumLit (loc 3))) (loc (NumLit (loc 5)))
                let env = checkEnvWithVar "dummy" TypeInt
                let result = compileExpr env M.empty M.empty expr
                result `shouldBe` Right [IR.PushInt 3, IR.PushInt 5, IR.LtInt]

            it "compiles integer greater-than" $ do
                let expr = BinOp Gt (loc (NumLit (loc 7))) (loc (NumLit (loc 3)))
                let env = checkEnvWithVar "dummy" TypeInt
                let result = compileExpr env M.empty M.empty expr
                result `shouldBe` Right [IR.PushInt 7, IR.PushInt 3, IR.GtInt]

            it "compiles integer less-than-or-equal" $ do
                let expr = BinOp Le (loc (NumLit (loc 5))) (loc (NumLit (loc 5)))
                let env = checkEnvWithVar "dummy" TypeInt
                let result = compileExpr env M.empty M.empty expr
                result `shouldBe` Right [IR.PushInt 5, IR.PushInt 5, IR.LeInt]

            it "compiles integer greater-than-or-equal" $ do
                let expr = BinOp Ge (loc (NumLit (loc 10))) (loc (NumLit (loc 5)))
                let env = checkEnvWithVar "dummy" TypeInt
                let result = compileExpr env M.empty M.empty expr
                result `shouldBe` Right [IR.PushInt 10, IR.PushInt 5, IR.GeInt]

        describe "Logical Operations" $ do
            it "compiles logical AND" $ do
                let expr = BinOp And (loc (BoolLit (loc True))) (loc (BoolLit (loc False)))
                let env = checkEnvWithVar "dummy" TypeBool
                let result = compileExpr env M.empty M.empty expr
                result `shouldBe` Right [IR.PushBool True, IR.PushBool False, IR.AndBool]

            it "compiles logical OR" $ do
                let expr = BinOp Or (loc (BoolLit (loc True))) (loc (BoolLit (loc False)))
                let env = checkEnvWithVar "dummy" TypeBool
                let result = compileExpr env M.empty M.empty expr
                result `shouldBe` Right [IR.PushBool True, IR.PushBool False, IR.OrBool]

        describe "Unary Operations" $ do
            it "compiles integer negation" $ do
                let expr = UnOp Neg (loc (NumLit (loc 5)))
                let env = checkEnvWithVar "dummy" TypeInt
                let result = compileExpr env M.empty M.empty expr
                result `shouldBe` Right [IR.PushInt 5, IR.NegInt]

            it "compiles float negation" $ do
                let expr = UnOp Neg (loc (FloatLit (loc 3.14)))
                let env = checkEnvWithVar "dummy" TypeFloat
                let result = compileExpr env M.empty M.empty expr
                result `shouldBe` Right [IR.PushFloat 3.14, IR.NegFloat]

            it "compiles boolean NOT" $ do
                let expr = UnOp Not (loc (BoolLit (loc True)))
                let env = checkEnvWithVar "dummy" TypeBool
                let result = compileExpr env M.empty M.empty expr
                result `shouldBe` Right [IR.PushBool True, IR.NotBool]

        describe "Function Calls" $ do
            it "compiles function call with no arguments" $ do
                let expr = Call (loc "foo") []
                let funcTable = M.fromList [("foo", 0)]
                let result = compileExpr emptyCheckEnv funcTable M.empty expr
                result `shouldBe` Right [IR.Call 0]

            it "compiles function call with single argument" $ do
                let expr = Call (loc "double") [NumLit (loc 5)]
                let funcTable = M.fromList [("double", 0)]
                let result = compileExpr emptyCheckEnv funcTable M.empty expr
                result `shouldBe` Right [IR.PushInt 5, IR.Call 0]

            it "compiles function call with multiple arguments" $ do
                let expr = Call (loc "add") [NumLit (loc 3), NumLit (loc 7)]
                let funcTable = M.fromList [("add", 0)]
                let result = compileExpr emptyCheckEnv funcTable M.empty expr
                result `shouldBe` Right [IR.PushInt 3, IR.PushInt 7, IR.Call 0]

        describe "Nested Expressions" $ do
            it "compiles nested arithmetic: (5 + 3) * 2" $ do
                let inner = BinOp Add (loc (NumLit (loc 5))) (loc (NumLit (loc 3)))
                let expr = BinOp Mul (loc inner) (loc (NumLit (loc 2)))
                let env = checkEnvWithVar "dummy" TypeInt
                let result = compileExpr env M.empty M.empty expr
                result `shouldBe` Right [IR.PushInt 5, IR.PushInt 3, IR.AddInt, IR.PushInt 2, IR.MulInt]

            it "compiles deeply nested expression: ((a + b) * c) - d" $ do
                let varTable = M.fromList [("a", 0), ("b", 1), ("c", 2), ("d", 3)]
                let env = foldr (\name e -> SE.markInitialized name $ SE.addVar name TypeInt e) emptyCheckEnv ["a", "b", "c", "d"]
                let inner1 = BinOp Add (loc (Var (loc "a"))) (loc (Var (loc "b")))
                let inner2 = BinOp Mul (loc inner1) (loc (Var (loc "c")))
                let expr = BinOp Sub (loc inner2) (loc (Var (loc "d")))
                let result = compileExpr env M.empty varTable expr
                result `shouldBe` Right [IR.GetLocal 0, IR.GetLocal 1, IR.AddInt, IR.GetLocal 2, IR.MulInt, IR.GetLocal 3, IR.SubInt]

-- ============================================================================
-- Compiler.Statement Tests
-- ============================================================================

    describe "Compiler.Statement" $ do

        describe "Variable Declarations" $ do
            it "compiles declaration without initialization" $ do
                let stmt = Decl TypeInt "x" Nothing
                let result = compileStatement emptyCheckEnv M.empty M.empty stmt
                result `shouldBe` Right []

            it "compiles declaration with integer initialization" $ do
                let stmt = Decl TypeInt "x" (Just (NumLit (loc 42)))
                let varTable = M.fromList [("x", 0)]
                let env = checkEnvWithVar "x" TypeInt
                let result = compileStatement env M.empty varTable stmt
                result `shouldBe` Right [IR.PushInt 42, IR.SetLocal 0]

            it "compiles declaration with expression initialization" $ do
                let initExpr = BinOp Add (loc (NumLit (loc 5))) (loc (NumLit (loc 3)))
                let stmt = Decl TypeInt "sum" (Just initExpr)
                let varTable = M.fromList [("sum", 0)]
                let env = checkEnvWithVar "sum" TypeInt
                let result = compileStatement env M.empty varTable stmt
                result `shouldBe` Right [IR.PushInt 5, IR.PushInt 3, IR.AddInt, IR.SetLocal 0]

        describe "Assignment Statements" $ do
            it "compiles simple assignment" $ do
                let stmt = Assign "x" (NumLit (loc 10))
                let varTable = M.fromList [("x", 0)]
                let env = checkEnvWithVar "x" TypeInt
                let result = compileStatement env M.empty varTable stmt
                result `shouldBe` Right [IR.PushInt 10, IR.SetLocal 0]

            it "compiles assignment with expression" $ do
                let expr = BinOp Mul (loc (NumLit (loc 3))) (loc (NumLit (loc 4)))
                let stmt = Assign "y" expr
                let varTable = M.fromList [("y", 0)]
                let env = checkEnvWithVar "y" TypeInt
                let result = compileStatement env M.empty varTable stmt
                result `shouldBe` Right [IR.PushInt 3, IR.PushInt 4, IR.MulInt, IR.SetLocal 0]

        describe "If Statements (Simple)" $ do
            it "compiles simple if with true condition" $ do
                let cond = BoolLit (loc True)
                let thenBody = [Return (NumLit (loc 42))]
                let stmt = If cond thenBody Nothing
                let env = checkEnvWithVar "dummy" TypeBool
                let result = compileStatement env M.empty M.empty stmt
                result `shouldBe` Right [IR.PushBool True, IR.JumpIfFalse 3, IR.PushInt 42, IR.Return]

            it "compiles simple if with empty body" $ do
                let cond = BoolLit (loc True)
                let thenBody = []
                let stmt = If cond thenBody Nothing
                let env = checkEnvWithVar "dummy" TypeBool
                let result = compileStatement env M.empty M.empty stmt
                result `shouldBe` Right [IR.PushBool True, IR.JumpIfFalse 1]

        describe "If-Else Statements" $ do
            it "compiles if-else with both branches" $ do
                let cond = BoolLit (loc True)
                let thenBody = [Return (NumLit (loc 42))]
                let elseBody = [Return (NumLit (loc 99))]
                let stmt = If cond thenBody (Just elseBody)
                let env = checkEnvWithVar "dummy" TypeBool
                let result = compileStatement env M.empty M.empty stmt
                result `shouldBe` Right [IR.PushBool True, IR.JumpIfFalse 4, IR.PushInt 42, IR.Return, IR.Jump 3, IR.PushInt 99, IR.Return]

            it "compiles if-else with empty else" $ do
                let cond = BoolLit (loc False)
                let thenBody = [Return (NumLit (loc 1))]
                let elseBody = []
                let stmt = If cond thenBody (Just elseBody)
                let env = checkEnvWithVar "dummy" TypeBool
                let result = compileStatement env M.empty M.empty stmt
                result `shouldBe` Right [IR.PushBool False, IR.JumpIfFalse 4, IR.PushInt 1, IR.Return, IR.Jump 1]

        describe "While Loops" $ do
            it "compiles simple while loop" $ do
                let cond = BoolLit (loc True)
                let body = [ExprStmt (NumLit (loc 1))]
                let stmt = While cond body
                let env = checkEnvWithVar "dummy" TypeBool
                let result = compileStatement env M.empty M.empty stmt
                -- cond, JumpIfFalse, body (PushInt, Pop), Jump (back)
                result `shouldBe` Right [IR.PushBool True, IR.JumpIfFalse 4, IR.PushInt 1, IR.Pop, IR.Jump (-4)]

            it "compiles while loop with empty body" $ do
                let cond = BoolLit (loc False)
                let body = []
                let stmt = While cond body
                let env = checkEnvWithVar "dummy" TypeBool
                let result = compileStatement env M.empty M.empty stmt
                result `shouldBe` Right [IR.PushBool False, IR.JumpIfFalse 2, IR.Jump (-2)]

        describe "For Loops" $ do
            it "compiles for loop with all parts" $ do
                let initStmt = Just (Decl TypeInt "i" (Just (NumLit (loc 0))))
                let condExpr = Just (BoolLit (loc True))
                let updateStmt = Just (Assign "i" (BinOp Add (loc (Var (loc "i"))) (loc (NumLit (loc 1)))))
                let body = [ExprStmt (NumLit (loc 1))]
                let stmt = For initStmt condExpr updateStmt body
                let varTable = M.fromList [("i", 0)]
                let env = checkEnvWithVar "i" TypeInt
                case compileStatement env M.empty varTable stmt of
                    Right instructions -> length instructions `shouldSatisfy` (> 0)
                    Left _ -> expectationFailure "Expected successful compilation"

            it "compiles for loop with no init" $ do
                let initStmt = Nothing
                let condExpr = Just (BoolLit (loc True))
                let updateStmt = Nothing
                let body = []
                let stmt = For initStmt condExpr updateStmt body
                let env = checkEnvWithVar "dummy" TypeBool
                let result = compileStatement env M.empty M.empty stmt
                result `shouldBe` Right [IR.PushBool True, IR.JumpIfFalse 2, IR.Jump (-2)]

            it "compiles for loop with no condition (infinite loop)" $ do
                let initStmt = Nothing
                let condExpr = Nothing  -- Infinite loop
                let updateStmt = Nothing
                let body = []
                let stmt = For initStmt condExpr updateStmt body
                let result = compileStatement emptyCheckEnv M.empty M.empty stmt
                result `shouldBe` Right [IR.PushBool True, IR.JumpIfFalse 2, IR.Jump (-2)]

        describe "Return Statements" $ do
            it "compiles return with integer literal" $ do
                let stmt = Return (NumLit (loc 42))
                let result = compileStatement emptyCheckEnv M.empty M.empty stmt
                result `shouldBe` Right [IR.PushInt 42, IR.Return]

            it "compiles return with expression" $ do
                let expr = BinOp Add (loc (NumLit (loc 10))) (loc (NumLit (loc 20)))
                let stmt = Return expr
                let env = checkEnvWithVar "dummy" TypeInt
                let result = compileStatement env M.empty M.empty stmt
                result `shouldBe` Right [IR.PushInt 10, IR.PushInt 20, IR.AddInt, IR.Return]

        describe "Expression Statements" $ do
            it "compiles expression statement (result discarded)" $ do
                let stmt = ExprStmt (NumLit (loc 42))
                let result = compileStatement emptyCheckEnv M.empty M.empty stmt
                result `shouldBe` Right [IR.PushInt 42, IR.Pop]

            it "compiles function call as statement" $ do
                let stmt = ExprStmt (Call (loc "foo") [])
                let funcTable = M.fromList [("foo", 0)]
                let result = compileStatement emptyCheckEnv funcTable M.empty stmt
                result `shouldBe` Right [IR.Call 0, IR.Pop]

        describe "Multiple Statements" $ do
            it "compiles sequence of statements" $ do
                let stmts = [ Decl TypeInt "x" (Just (NumLit (loc 5)))
                            , Assign "x" (NumLit (loc 10))
                            , Return (Var (loc "x")) ]
                let varTable = M.fromList [("x", 0)]
                let env = checkEnvWithVar "x" TypeInt
                let result = compileStatements env M.empty varTable stmts
                result `shouldBe` Right [IR.PushInt 5, IR.SetLocal 0, IR.PushInt 10, IR.SetLocal 0, IR.GetLocal 0, IR.Return]

            it "compiles empty statement list" $ do
                let stmts = []
                let result = compileStatements emptyCheckEnv M.empty M.empty stmts
                result `shouldBe` Right []

        describe "Edge Cases - Complex Control Flow" $ do
            it "compiles nested if statements" $ do
                let innerIf = If (BoolLit (loc True)) [Return (NumLit (loc 1))] Nothing
                let outerIf = If (BoolLit (loc False)) [innerIf] Nothing
                let env = checkEnvWithVar "dummy" TypeBool
                case compileStatement env M.empty M.empty outerIf of
                    Right instructions -> length instructions `shouldSatisfy` (> 0)
                    Left _ -> expectationFailure "Expected successful compilation"

            it "compiles if inside while loop" $ do
                let innerIf = If (BoolLit (loc True)) [Return (NumLit (loc 1))] Nothing
                let whileStmt = While (BoolLit (loc True)) [innerIf]
                let env = checkEnvWithVar "dummy" TypeBool
                case compileStatement env M.empty M.empty whileStmt of
                    Right instructions -> length instructions `shouldSatisfy` (> 0)
                    Left _ -> expectationFailure "Expected successful compilation"

-- ============================================================================
-- Compiler.Function Tests
-- ============================================================================

    describe "Compiler.Function" $ do

        describe "Function Compilation" $ do
            it "compiles simple function with no parameters" $ do
                let func = simpleFunc "main" [] [Return (NumLit (loc 42))]
                let funcTable = M.fromList [("main", 0)]
                let funcSigs = M.fromList [("main", (TypeInt, []))]
                let env = SE.makeFunctionEnv funcSigs func
                case compileFunction env funcTable func of
                    Right compiled -> do
                        funcName compiled `shouldBe` "main"
                        paramCount compiled `shouldBe` 0
                        localVarCount compiled `shouldBe` 0
                        code compiled `shouldBe` [IR.PushInt 42, IR.Return]
                    Left _ -> expectationFailure "Expected successful compilation"

            it "compiles function with parameters" $ do
                let params = [Parameter TypeInt "a", Parameter TypeInt "b"]
                let body = [Return (BinOp Add (loc (Var (loc "a"))) (loc (Var (loc "b"))))]
                let func = simpleFunc "add" params body
                let funcTable = M.fromList [("add", 0)]
                let funcSigs = M.fromList [("add", (TypeInt, [TypeInt, TypeInt]))]
                let env = SE.makeFunctionEnv funcSigs func
                case compileFunction env funcTable func of
                    Right compiled -> do
                        funcName compiled `shouldBe` "add"
                        paramCount compiled `shouldBe` 2
                        localVarCount compiled `shouldBe` 0
                        code compiled `shouldBe` [IR.GetLocal 0, IR.GetLocal 1, IR.AddInt, IR.Return]
                    Left _ -> expectationFailure "Expected successful compilation"

            it "compiles function with local variables" $ do
                let body = [ Decl TypeInt "x" (Just (NumLit (loc 5)))
                           , Return (Var (loc "x")) ]
                let func = simpleFunc "test" [] body
                let funcTable = M.fromList [("test", 0)]
                let funcSigs = M.fromList [("test", (TypeInt, []))]
                let env = SE.makeFunctionEnv funcSigs func
                case compileFunction env funcTable func of
                    Right compiled -> do
                        funcName compiled `shouldBe` "test"
                        paramCount compiled `shouldBe` 0
                        localVarCount compiled `shouldBe` 1
                        code compiled `shouldBe` [IR.PushInt 5, IR.SetLocal 0, IR.GetLocal 0, IR.Return]
                    Left _ -> expectationFailure "Expected successful compilation"

            it "compiles function with parameters and locals (indices are correct)" $ do
                let params = [Parameter TypeInt "x"]
                let body = [ Decl TypeInt "y" (Just (NumLit (loc 10)))
                           , Return (BinOp Add (loc (Var (loc "x"))) (loc (Var (loc "y")))) ]
                let func = simpleFunc "test" params body
                let funcTable = M.fromList [("test", 0)]
                let funcSigs = M.fromList [("test", (TypeInt, [TypeInt]))]
                let env = SE.makeFunctionEnv funcSigs func
                case compileFunction env funcTable func of
                    Right compiled -> do
                        paramCount compiled `shouldBe` 1
                        localVarCount compiled `shouldBe` 1
                        code compiled `shouldBe` [IR.PushInt 10, IR.SetLocal 1, IR.GetLocal 0, IR.GetLocal 1, IR.AddInt, IR.Return]
                    Left _ -> expectationFailure "Expected successful compilation"

-- ============================================================================
-- Compiler.Core Tests (Full Program Compilation)
-- ============================================================================

    describe "Compiler.Core" $ do

        describe "Complete Program Compilation" $ do
            it "compiles simple program with main" $ do
                let source = "int main() { return 42; }"
                case parseProgram source of
                    Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                    Right ast -> case checkProgram ast of
                        Left typeErrs -> expectationFailure $ "Type check failed: " ++ show typeErrs
                        Right validatedAst -> case compileProgram validatedAst of
                            Left compileErr -> expectationFailure $ "Compile failed: " ++ show compileErr
                            Right irProgram -> do
                                length (functions irProgram) `shouldBe` 1
                                funcName (head (functions irProgram)) `shouldBe` "main"
                                mainIndex irProgram `shouldBe` 0

            it "compiles program with multiple functions" $ do
                let source = "int add(int a, int b) { return a + b; } int main() { return add(5, 3); }"
                case parseProgram source of
                    Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                    Right ast -> case checkProgram ast of
                        Left typeErrs -> expectationFailure $ "Type check failed: " ++ show typeErrs
                        Right validatedAst -> case compileProgram validatedAst of
                            Left compileErr -> expectationFailure $ "Compile failed: " ++ show compileErr
                            Right irProgram -> do
                                length (functions irProgram) `shouldBe` 2
                                mainIndex irProgram `shouldBe` 1

            it "compiles program with local variables" $ do
                let source = "int main() { int x = 5; int y = 10; return x + y; }"
                case parseProgram source of
                    Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                    Right ast -> case checkProgram ast of
                        Left typeErrs -> expectationFailure $ "Type check failed: " ++ show typeErrs
                        Right validatedAst -> case compileProgram validatedAst of
                            Left compileErr -> expectationFailure $ "Compile failed: " ++ show compileErr
                            Right irProgram -> do
                                let mainFunc = functions irProgram !! mainIndex irProgram
                                localVarCount mainFunc `shouldBe` 2

-- ============================================================================
-- Edge Case Tests
-- ============================================================================

    describe "Edge Cases" $ do

        describe "Complex Expressions" $ do
            it "compiles expression with mixed operations" $ do
                let source = "int main() { return (5 + 3) * 2 - 1; }"
                case parseProgram source of
                    Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                    Right ast -> case checkProgram ast of
                        Left typeErrs -> expectationFailure $ "Type check failed: " ++ show typeErrs
                        Right validatedAst -> case compileProgram validatedAst of
                            Left _ -> expectationFailure "Expected successful compilation"
                            Right _ -> return ()  -- Success

            it "compiles deeply nested expression" $ do
                let source = "int main() { return ((((1 + 2) * 3) - 4) / 5); }"
                case parseProgram source of
                    Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                    Right ast -> case checkProgram ast of
                        Left typeErrs -> expectationFailure $ "Type check failed: " ++ show typeErrs
                        Right validatedAst -> case compileProgram validatedAst of
                            Left _ -> expectationFailure "Expected successful compilation"
                            Right _ -> return ()  -- Success

        describe "Complex Control Flow" $ do
            it "compiles recursive function (factorial)" $ do
                let source = "int fact(int n) { if (n <= 1) { return 1; } return n * fact(n - 1); } int main() { return fact(5); }"
                case parseProgram source of
                    Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                    Right ast -> case checkProgram ast of
                        Left typeErrs -> expectationFailure $ "Type check failed: " ++ show typeErrs
                        Right validatedAst -> case compileProgram validatedAst of
                            Left _ -> expectationFailure "Expected successful compilation"
                            Right irProgram -> length (functions irProgram) `shouldBe` 2

            it "compiles nested loops" $ do
                let source = "int main() { int sum = 0; int i = 0; while (i < 10) { int j = 0; while (j < 10) { sum = sum + 1; j = j + 1; } i = i + 1; } return sum; }"
                case parseProgram source of
                    Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                    Right ast -> case checkProgram ast of
                        Left typeErrs -> expectationFailure $ "Type check failed: " ++ show typeErrs
                        Right validatedAst -> case compileProgram validatedAst of
                            Left _ -> expectationFailure "Expected successful compilation"
                            Right _ -> return ()  -- Success

        describe "Boundary Conditions" $ do
            it "compiles function with zero" $ do
                let source = "int main() { return 0; }"
                case parseProgram source of
                    Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                    Right ast -> case checkProgram ast of
                        Left typeErrs -> expectationFailure $ "Type check failed: " ++ show typeErrs
                        Right validatedAst -> case compileProgram validatedAst of
                            Left _ -> expectationFailure "Expected successful compilation"
                            Right _ -> return ()  -- Success

            it "compiles function with negative numbers" $ do
                let source = "int main() { return -42; }"
                case parseProgram source of
                    Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                    Right ast -> case checkProgram ast of
                        Left typeErrs -> expectationFailure $ "Type check failed: " ++ show typeErrs
                        Right validatedAst -> case compileProgram validatedAst of
                            Left _ -> expectationFailure "Expected successful compilation"
                            Right _ -> return ()  -- Success

            it "compiles function with large numbers" $ do
                let source = "int main() { return 2147483647; }"
                case parseProgram source of
                    Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                    Right ast -> case checkProgram ast of
                        Left typeErrs -> expectationFailure $ "Type check failed: " ++ show typeErrs
                        Right validatedAst -> case compileProgram validatedAst of
                            Left _ -> expectationFailure "Expected successful compilation"
                            Right _ -> return ()  -- Success

        describe "Float Operations" $ do
            it "compiles float arithmetic" $ do
                let source = "float main() { return 3.14 + 2.86; }"
                case parseProgram source of
                    Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                    Right ast -> case checkProgram ast of
                        Left typeErrs -> expectationFailure $ "Type check failed: " ++ show typeErrs
                        Right validatedAst -> case compileProgram validatedAst of
                            Left _ -> expectationFailure "Expected successful compilation"
                            Right _ -> return ()  -- Success

            it "compiles float comparison" $ do
                let source = "int main() { if (3.14 > 2.0) { return 1; } return 0; }"
                case parseProgram source of
                    Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                    Right ast -> case checkProgram ast of
                        Left typeErrs -> expectationFailure $ "Type check failed: " ++ show typeErrs
                        Right validatedAst -> case compileProgram validatedAst of
                            Left _ -> expectationFailure "Expected successful compilation"
                            Right _ -> return ()  -- Success

        describe "Multiple Functions with Calls" $ do
            it "compiles program with function call chain" $ do
                let source = "int a() { return 1; } int b() { return a() + 1; } int c() { return b() + 1; } int main() { return c(); }"
                case parseProgram source of
                    Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                    Right ast -> case checkProgram ast of
                        Left typeErrs -> expectationFailure $ "Type check failed: " ++ show typeErrs
                        Right validatedAst -> case compileProgram validatedAst of
                            Left _ -> expectationFailure "Expected successful compilation"
                            Right irProgram -> length (functions irProgram) `shouldBe` 4

            it "compiles program with multiple parameters" $ do
                let source = "int sum3(int a, int b, int c) { return a + b + c; } int main() { return sum3(1, 2, 3); }"
                case parseProgram source of
                    Left parseErr -> expectationFailure $ "Parse failed: " ++ show parseErr
                    Right ast -> case checkProgram ast of
                        Left typeErrs -> expectationFailure $ "Type check failed: " ++ show typeErrs
                        Right validatedAst -> case compileProgram validatedAst of
                            Left _ -> expectationFailure "Expected successful compilation"
                            Right irProgram -> do
                                let sumFunc = head (functions irProgram)
                                paramCount sumFunc `shouldBe` 3
