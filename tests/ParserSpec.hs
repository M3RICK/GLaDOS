{-# LANGUAGE OverloadedStrings #-}
module ParserSpec (spec) where

import Test.Hspec
import Data.Either (isLeft)

import Types
import qualified Parser.Atomic   as A
import qualified Parser.Core     as C
import qualified Parser.Compound as Comp

import Text.Parsec (parse, ParseError)

-- Helpers to simplify checking parse results
expectRight :: Either ParseError LispVal -> LispVal
expectRight (Right v) = v
expectRight (Left e)  = error $ "Unexpected parse error: " ++ show e

spec :: Spec
spec = do
  describe "Atomic parsers" $ do
    it "parses positive numbers" $
      parse A.parseNumber "" "42" `shouldBe` Right (Number 42)

    it "parses negative numbers" $
      parse A.parseNumber "" "-99" `shouldBe` Right (Number (-99))

    it "parses booleans #t and #f" $ do
      parse A.parseBool "" "#t" `shouldBe` Right (Bool True)
      parse A.parseBool "" "#f" `shouldBe` Right (Bool False)

    it "parses simple symbols" $
      parse A.parseSymbol "" "foo" `shouldBe` Right (Atom "foo")

    it "parses operator symbols" $
      parse A.parseSymbol "" "+" `shouldBe` Right (Atom "+")


  describe "Compound parsers" $ do
    it "parses an empty list" $
      parse (Comp.parseList C.parseExpr) "" "()" `shouldBe` Right (List [])

    it "parses a list of numbers" $
      parse (Comp.parseList C.parseExpr) "" "(1 2 3)" `shouldBe`
        Right (List [Number 1, Number 2, Number 3])

    it "parses nested lists" $
      parse (Comp.parseList C.parseExpr) "" "((a b) (1 2))" `shouldBe`
        Right (List [ List [Atom "a", Atom "b"]
                    , List [Number 1, Number 2] ])


  describe "Core parser" $ do
    it "parses a simple number with readExpr" $
      expectRight (C.readExpr "123") `shouldBe` Number 123

    it "parses a symbol with readExpr" $
      expectRight (C.readExpr "foo") `shouldBe` Atom "foo"

    it "parses whitespace-insensitive list" $
      expectRight (C.readExpr " (  42   foo ) ") `shouldBe`
        List [Number 42, Atom "foo"]

    it "fails on malformed input" $
      C.readExpr "(1 2" `shouldSatisfy` isLeft
