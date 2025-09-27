-- Tests pour le Compiler
-- Auteur : Copilot (Epitech style)

module CompilerSpec (spec) where

import Test.Hspec
import Compiler
import Parser (Expression(..), Statement(..), ArithOp(..), BoolOp(..))
import VirtualMachine (Value(..), Instruction(..), runVM)

spec :: Spec
spec = describe "Compiler Tests" $ do
    
    -- Tests de base
    basicCompilationTests
    
    -- Tests d'expressions arithmétiques
    arithmeticCompilationTests
    
    -- Tests d'expressions booléennes  
    booleanCompilationTests
    
    -- Tests du pipeline complet
    pipelineTests

-- | Tests de compilation de base
basicCompilationTests :: Spec
basicCompilationTests = describe "Basic Compilation" $ do
    
    it "should compile integer literal" $ do
        let instructions = compileExpression (EInt 42)
        instructions `shouldBe` [Push (VInt 42)]
    
    it "should compile boolean literal" $ do
        let instructions = compileExpression (EBool True)
        instructions `shouldBe` [Push (VBool True)]
    
    it "should compile variable reference" $ do
        let instructions = compileExpression (EVar "x")
        instructions `shouldBe` [Load "x"]

-- | Tests d'expressions arithmétiques
arithmeticCompilationTests :: Spec
arithmeticCompilationTests = describe "Arithmetic Compilation" $ do
    
    it "should compile simple addition" $ do
        let expr = EArith (EInt 1) Add (EInt 2)
        let instructions = compileExpression expr
        instructions `shouldBe` [Push (VInt 1), Push (VInt 2), AddI]
    
    it "should compile subtraction" $ do
        let expr = EArith (EInt 5) Sub (EInt 3)
        let instructions = compileExpression expr
        instructions `shouldBe` [Push (VInt 5), Push (VInt 3), SubI]
    
    it "should compile multiplication" $ do
        let expr = EArith (EInt 4) Mul (EInt 3)
        let instructions = compileExpression expr
        instructions `shouldBe` [Push (VInt 4), Push (VInt 3), MulI]
    
    it "should compile division" $ do
        let expr = EArith (EInt 8) Div (EInt 2)
        let instructions = compileExpression expr
        instructions `shouldBe` [Push (VInt 8), Push (VInt 2), DivI]
    
    it "should compile complex expression (1 + 2) * 3" $ do
        let expr = EArith (EArith (EInt 1) Add (EInt 2)) Mul (EInt 3)
        let instructions = compileExpression expr
        instructions `shouldBe` [Push (VInt 1), Push (VInt 2), AddI, Push (VInt 3), MulI]

-- | Tests d'expressions booléennes
booleanCompilationTests :: Spec
booleanCompilationTests = describe "Boolean Compilation" $ do
    
    it "should compile equality comparison" $ do
        let expr = EBoolOp (EInt 5) Eq (EInt 5)
        let instructions = compileExpression expr
        instructions `shouldBe` [Push (VInt 5), Push (VInt 5), EqI]
    
    it "should compile less than comparison" $ do
        let expr = EBoolOp (EInt 3) Lt (EInt 5)
        let instructions = compileExpression expr
        instructions `shouldBe` [Push (VInt 3), Push (VInt 5), LtI]
    
    it "should compile logical AND" $ do
        let expr = EBoolOp (EBool True) And (EBool False)
        let instructions = compileExpression expr
        instructions `shouldBe` [Push (VBool True), Push (VBool False), AndB]

-- | Tests du pipeline complet
pipelineTests :: Spec  
pipelineTests = describe "Full Pipeline" $ do
    
    it "should compile and execute simple arithmetic" $ do
        let expr = EArith (EInt 10) Add (EInt 5)
        compileAndRun expr `shouldBe` Right (VInt 15)
    
    it "should compile and execute boolean expression" $ do
        let expr = EBoolOp (EInt 3) Lt (EInt 5)
        compileAndRun expr `shouldBe` Right (VBool True)
    
    it "should compile and execute complex expression" $ do
        let expr = EArith (EArith (EInt 2) Mul (EInt 3)) Add (EInt 4)  -- (2*3)+4 = 10
        compileAndRun expr `shouldBe` Right (VInt 10)
    
    it "should handle division by zero through VM" $ do
        let expr = EArith (EInt 10) Div (EInt 0)
        case compileAndRun expr of
            Left (VMExecutionError _) -> True `shouldBe` True  -- Expected VM error
            _ -> False `shouldBe` True  -- Should not succeed

-- Tests rapides avec les fonctions utilitaires
quickTests :: Spec
quickTests = describe "Quick Utility Tests" $ do
    
    it "should test arithmetic helper" $ do
        let instructions = testArithmetic 6 3 Mul
        case runVM instructions of
            Right (VInt 18) -> True `shouldBe` True
            _ -> False `shouldBe` True
    
    it "should test boolean helper" $ do
        let instructions = testBoolean 5 3 Gt
        case runVM instructions of
            Right (VBool True) -> True `shouldBe` True
            _ -> False `shouldBe` True