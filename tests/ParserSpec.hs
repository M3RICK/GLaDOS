module ParserSpec (spec) where

import Test.Hspec
import Parser (parseProgram, parseExpression, AST(..), Expression(..), ArithOp(..), BoolOp(..))

-- Tests de compatibilité (existants)
basicTestCases :: [(String, Either String AST)]
basicTestCases =
  [ ("42", Right (ASTInt 42))
  , ("true", Right (ASTBool True))
  , ("false", Right (ASTBool False))
  , ("", Left "Parse error: not a valid int or bool")
  , ("abc", Right (ASTExpression (EVar "abc")))  -- identifiant valide maintenant
  ]

-- Tests étendus pour le nouveau parser
extendedTestCases :: [(String, Either String AST)]
extendedTestCases =
  [ ("variable", Right (ASTExpression (EVar "variable")))
  , ("_underscore", Right (ASTExpression (EVar "_underscore")))
  , ("var123", Right (ASTExpression (EVar "var123")))
  , ("123abc", Left "Parse error: not a valid int or bool")  -- identifiant invalide
  , ("123", Right (ASTInt 123))
  , ("0", Right (ASTInt 0))
  ]

-- Tests pour expressions arithmétiques
arithmeticTestCases :: [(String, Either String (Expression, [String]))]
arithmeticTestCases =
  [ ("1 + 2", Right (EArith (EInt 1) Add (EInt 2), []))
  , ("3 - 4", Right (EArith (EInt 3) Sub (EInt 4), []))
  , ("5 * 6", Right (EArith (EInt 5) Mul (EInt 6), []))
  , ("8 / 2", Right (EArith (EInt 8) Div (EInt 2), []))
  , ("x + y", Right (EArith (EVar "x") Add (EVar "y"), []))
  , ("1 + 2 * 3", Right (EArith (EInt 1) Add (EArith (EInt 2) Mul (EInt 3)), []))
  , ("true && false", Right (EBoolOp (EBool True) And (EBool False), []))
  , ("x == y", Right (EBoolOp (EVar "x") Eq (EVar "y"), []))
  ]

spec :: Spec
spec = describe "Parser" $ do
  describe "parseProgram (basic compatibility)" $ do
    mapM_ (\(input, expected) ->
      it ("parses '" ++ input ++ "'") $ do
        parseProgram input `shouldBe` expected
      ) basicTestCases
  
  describe "parseProgram (extended features)" $ do
    mapM_ (\(input, expected) ->
      it ("parses '" ++ input ++ "'") $ do
        parseProgram input `shouldBe` expected
      ) extendedTestCases
      
  describe "parseExpression (arithmetic and boolean)" $ do
    mapM_ (\(input, expected) ->
      it ("parses '" ++ input ++ "'") $ do
        parseExpression (words input) `shouldBe` expected
      ) arithmeticTestCases
