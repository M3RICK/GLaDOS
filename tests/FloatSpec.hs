module FloatSpec (spec) where

import Test.Hspec
import Text.Megaparsec (runParser)
import Parser.Expr (pExpr)
import AST.AST

spec :: Spec
spec = describe "float literals" $ do
  it "parses 3.14 as FloatLit" $ do
    case runParser pExpr "<test>" "3.14" of
      Right (FloatLit (Located _ v)) -> v `shouldBe` (3.14 :: Double)
      r -> expectationFailure ("unexpected: " ++ show r)

  it "parses -2.0e+1 as FloatLit (-20.0)" $ do
    case runParser pExpr "<test>" "-2.0e+1" of
      Right (FloatLit (Located _ v)) -> v `shouldBe` (-20.0 :: Double)
      r -> expectationFailure ("unexpected: " ++ show r)

  it "parses 2. as FloatLit 2.0" $ do
    case runParser pExpr "<test>" "2." of
      Right (FloatLit (Located _ v)) -> v `shouldBe` (2.0 :: Double)
      r -> expectationFailure ("unexpected: " ++ show r)

  it "keeps 42 as an Int NumLit (not a FloatLit)" $ do
    case runParser pExpr "<test>" "42" of
      Right (NumLit (Located _ n)) -> n `shouldBe` (42 :: Int)
      Right (FloatLit _)           -> expectationFailure "expected NumLit, got FloatLit"
      r -> expectationFailure ("unexpected: " ++ show r)
