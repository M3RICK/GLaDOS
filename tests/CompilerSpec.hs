module CompilerSpec where

import Test.Hspec
import Compiler.Compiler
import AST.AST
import qualified Language.Wasm.Structure as Wasm

spec :: Spec
spec = do
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