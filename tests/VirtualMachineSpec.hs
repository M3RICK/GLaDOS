{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-TLS-5-1-glados-2 [WSL: Ubuntu-22.04]
-- File description:
-- VirtualMachineSpec
-}

-- Tests unitaires pour la Virtual Machine
-- Description : Tests complets pour la VM stack-based

module VirtualMachineSpec (spec) where

import Test.Hspec
import VirtualMachine

spec :: Spec
spec = describe "Virtual Machine Tests" $ do
    stackBasicTests
    arithmeticTests
    booleanTests
    variableTests
    controlFlowTests
    errorHandlingTests

-- | Tests de base pour la manipulation de pile
stackBasicTests :: Spec
stackBasicTests = describe "Stack Operations" $ do
    
    it "should push and execute simple value" $ do
        let instructions = [Push (VInt 42), Halt]
        runVM instructions `shouldBe` Right (VInt 42)
    
    it "should push boolean value" $ do
        let instructions = [Push (VBool True), Halt]
        runVM instructions `shouldBe` Right (VBool True)
    
    it "should duplicate top of stack" $ do
        let instructions = [Push (VInt 10), Dup, Pop, Halt]
        runVM instructions `shouldBe` Right (VInt 10)
    
    it "should handle empty stack peek" $ do
        let state = emptyVMState []
        peekStack state `shouldBe` Left StackUnderflow
    
    it "should handle empty stack pop" $ do
        let state = emptyVMState []
        popStack state `shouldBe` Left StackUnderflow

-- | Tests des opérations arithmétiques
arithmeticTests :: Spec
arithmeticTests = describe "Arithmetic Operations" $ do
    
    it "should add two integers" $ do
        let instructions = [Push (VInt 10), Push (VInt 5), AddI, Halt]
        runVM instructions `shouldBe` Right (VInt 15)
    
    it "should subtract two integers" $ do
        let instructions = [Push (VInt 10), Push (VInt 3), SubI, Halt]
        runVM instructions `shouldBe` Right (VInt 7)
    
    it "should multiply two integers" $ do
        let instructions = [Push (VInt 6), Push (VInt 7), MulI, Halt]
        runVM instructions `shouldBe` Right (VInt 42)
    
    it "should divide two integers" $ do
        let instructions = [Push (VInt 20), Push (VInt 4), DivI, Halt]
        runVM instructions `shouldBe` Right (VInt 5)
    
    it "should handle division by zero" $ do
        let instructions = [Push (VInt 10), Push (VInt 0), DivI, Halt]
        runVM instructions `shouldBe` Left DivisionByZero
    
    it "should handle complex arithmetic expression" $ do
        let instructions = [Push (VInt 10), Push (VInt 5), AddI, Push (VInt 2), MulI, Halt]
        runVM instructions `shouldBe` Right (VInt 30)

-- | Tests des opérations booléennes
booleanTests :: Spec
booleanTests = describe "Boolean Operations" $ do
    
    it "should compare integers for equality (true)" $ do
        let instructions = [Push (VInt 5), Push (VInt 5), EqI, Halt]
        runVM instructions `shouldBe` Right (VBool True)
    
    it "should compare integers for equality (false)" $ do
        let instructions = [Push (VInt 5), Push (VInt 3), EqI, Halt]
        runVM instructions `shouldBe` Right (VBool False)
    
    it "should compare integers for less than" $ do
        let instructions = [Push (VInt 3), Push (VInt 5), LtI, Halt]
        runVM instructions `shouldBe` Right (VBool True)
    
    it "should compare integers for greater than" $ do
        let instructions = [Push (VInt 8), Push (VInt 3), GtI, Halt]
        runVM instructions `shouldBe` Right (VBool True)
    
    it "should perform logical AND (true)" $ do
        let instructions = [Push (VBool True), Push (VBool True), AndB, Halt]
        runVM instructions `shouldBe` Right (VBool True)
    
    it "should perform logical AND (false)" $ do
        let instructions = [Push (VBool True), Push (VBool False), AndB, Halt]
        runVM instructions `shouldBe` Right (VBool False)
    
    it "should perform logical OR (true)" $ do
        let instructions = [Push (VBool False), Push (VBool True), OrB, Halt]
        runVM instructions `shouldBe` Right (VBool True)
    
    it "should perform logical NOT" $ do
        let instructions = [Push (VBool True), NotB, Halt]
        runVM instructions `shouldBe` Right (VBool False)

-- | Tests des variables
variableTests :: Spec
variableTests = describe "Variable Operations" $ do
    
    it "should store and load integer variable" $ do
        let instructions = [Push (VInt 42), Store "x", Load "x", Halt]
        runVM instructions `shouldBe` Right (VInt 42)
    
    it "should store and load boolean variable" $ do
        let instructions = [Push (VBool True), Store "flag", Load "flag", Halt]
        runVM instructions `shouldBe` Right (VBool True)
    
    it "should handle undefined variable" $ do
        let instructions = [Load "undefined_var", Halt]
        runVM instructions `shouldBe` Left (UndefinedVariable "undefined_var")
    
    it "should update existing variable" $ do
        let instructions = [Push (VInt 10), Store "counter", Push (VInt 20), Store "counter", Load "counter", Halt]
        runVM instructions `shouldBe` Right (VInt 20)

-- | Tests de contrôle de flot
controlFlowTests :: Spec
controlFlowTests = describe "Control Flow" $ do
    
    it "should perform unconditional jump" $ do
        let instructions = [Push (VInt 1), Jump 3, Push (VInt 2), Halt]
        runVM instructions `shouldBe` Right (VInt 1)
    
    it "should perform conditional jump when true" $ do
        let instructions = [Push (VBool True), JumpIf 3, Push (VInt 1), Push (VInt 2), Halt]
        runVM instructions `shouldBe` Right (VInt 2)
    
    it "should not jump when condition is false" $ do
        let instructions = [Push (VBool False), JumpIf 4, Push (VInt 42), Halt]
        runVM instructions `shouldBe` Right (VInt 42)

-- | Tests de gestion d'erreurs
errorHandlingTests :: Spec
errorHandlingTests = describe "Error Handling" $ do
    
    it "should handle stack underflow on addition" $ do
        let instructions = [Push (VInt 5), AddI, Halt]
        runVM instructions `shouldBe` Left StackUnderflow
    
    it "should handle type mismatch in arithmetic" $ do
        let instructions = [Push (VBool True), Push (VInt 5), AddI, Halt]
        runVM instructions `shouldBe` Left (TypeMismatch "Expected integers for arithmetic operation")
    
    it "should handle type mismatch in boolean operations" $ do
        let instructions = [Push (VInt 5), Push (VBool True), AndB, Halt]
        runVM instructions `shouldBe` Left (TypeMismatch "Expected booleans for logical operation")
    
    it "should handle invalid jump address" $ do
        let instructions = [Jump 100, Halt]
        runVM instructions `shouldBe` Left (InvalidJump 100)
    
    it "should handle program counter out of bounds" $ do
        let instructions = []
        runVM instructions `shouldBe` Left (RuntimeError "Program counter out of bounds")

