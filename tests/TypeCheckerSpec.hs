{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-TLS-5-1-glados-2 [WSL: Ubuntu-22.04]
-- File description:
-- TypeCheckerSpec
-}

module TypeCheckerSpec (spec) where

import Test.Hspec
import TypeChecker (
    TypeError(..), TypeEnv,
    typeCheckExpression, typeCheckStatement,
    emptyEnv, addVar, lookupVar
    )
import Parser (Type(..), Expression(..), Statement(..), ArithOp(..), BoolOp(..))

spec :: Spec
spec = describe "TypeChecker" $ do
  describe "Environment operations" $ do
    it "adds variable to empty environment" $ do
      let result = addVar "x" TInt emptyEnv
      result `shouldBe` Right (fromList [("x", TInt)])
      
    it "fails to add duplicate variable" $ do
      let env = fromList [("x", TInt)]
      let result = addVar "x" TBool env
      result `shouldBe` Left (RedefinedVariable "x")
      
    it "looks up existing variable" $ do
      let env = fromList [("x", TInt)]
      lookupVar "x" env `shouldBe` Right TInt
      
    it "fails to lookup undefined variable" $ do
      lookupVar "y" emptyEnv `shouldBe` Left (UndefinedVariable "y")
  
  describe "Expression type checking" $ do
    it "checks integer literal" $ do
      typeCheckExpression emptyEnv (EInt 42) `shouldBe` Right TInt
      
    it "checks boolean literal" $ do
      typeCheckExpression emptyEnv (EBool True) `shouldBe` Right TBool
      
    it "checks variable reference" $ do
      let env = fromList [("x", TInt)]
      typeCheckExpression env (EVar "x") `shouldBe` Right TInt
      
    it "fails on undefined variable" $ do
      typeCheckExpression emptyEnv (EVar "x") `shouldBe` Left (UndefinedVariable "x")
      
    it "checks arithmetic expression" $ do
      let expr = EArith (EInt 1) Add (EInt 2)
      typeCheckExpression emptyEnv expr `shouldBe` Right TInt
      
    it "fails on type mismatch in arithmetic" $ do
      let expr = EArith (EInt 1) Add (EBool True)
      typeCheckExpression emptyEnv expr `shouldBe` Left (TypeMismatch TInt TBool "arithmetic Add")
      
    it "checks boolean expression" $ do
      let expr = EBoolOp (EBool True) And (EBool False)
      typeCheckExpression emptyEnv expr `shouldBe` Right TBool
      
    it "checks comparison expression" $ do
      let expr = EBoolOp (EInt 1) Lt (EInt 2)
      typeCheckExpression emptyEnv expr `shouldBe` Right TBool
      
    it "fails on type mismatch in comparison" $ do
      let expr = EBoolOp (EInt 1) Lt (EBool True)
      typeCheckExpression emptyEnv expr `shouldBe` Left (TypeMismatch TInt TBool "comparison")
      
    it "checks equality expression" $ do
      let expr = EBoolOp (EInt 1) Eq (EInt 2)
      typeCheckExpression emptyEnv expr `shouldBe` Right TBool
      
  describe "Statement type checking" $ do
    it "checks variable declaration" $ do
      let stmt = SDecl TInt "x"
      let result = typeCheckStatement emptyEnv stmt
      result `shouldBe` Right (fromList [("x", TInt)])
      
    it "fails on redeclaration" $ do
      let env = fromList [("x", TInt)]
      let stmt = SDecl TBool "x"
      typeCheckStatement env stmt `shouldBe` Left (RedefinedVariable "x")
      
    it "checks assignment" $ do
      let env = fromList [("x", TInt)]
      let stmt = SAssign "x" (EInt 42)
      typeCheckStatement env stmt `shouldBe` Right env
      
    it "fails on assignment type mismatch" $ do
      let env = fromList [("x", TInt)]
      let stmt = SAssign "x" (EBool True)
      typeCheckStatement env stmt `shouldBe` Left (TypeMismatch TInt TBool "assignment to x")
      
    it "fails on assignment to undefined variable" $ do
      let stmt = SAssign "x" (EInt 42)
      typeCheckStatement emptyEnv stmt `shouldBe` Left (UndefinedVariable "x")
      
    it "checks if statement" $ do
      let stmt = SIf (EBool True) [] []
      typeCheckStatement emptyEnv stmt `shouldBe` Right emptyEnv
      
    it "fails on non-boolean if condition" $ do
      let stmt = SIf (EInt 42) [] []
      typeCheckStatement emptyEnv stmt `shouldBe` Left (TypeMismatch TBool TInt "if condition")

-- Helper function to create a Map from list (for cleaner tests)
fromList :: [(String, Type)] -> TypeEnv
fromList = foldr (uncurry addVarUnsafe) emptyEnv
  where
    addVarUnsafe name typ env = case addVar name typ env of
      Right newEnv -> newEnv
      Left _ -> env  -- Should not happen in tests