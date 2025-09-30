module CompilerSpec where

import Test.Hspec
import Compiler.Compiler
import AST.AST
import qualified Language.Wasm.Structure as Wasm
import qualified Data.Text.Lazy as T

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


    describe "compileFunc" $ do
        it "compiles a function returning 5 + 3" $ do
            let func = Function
                    { fType = TypeInt
                    , fName = "test"
                    , fParams = []
                    , fBody = [Return (BinOp Add (NumLit 5) (NumLit 3))]
                    }
            let Wasm.Function _ _ resultBody = compileFunc 0 func
            resultBody `shouldBe` [Wasm.I64Const 5, Wasm.I64Const 3, Wasm.IBinOp Wasm.BS64 Wasm.IAdd, Wasm.Return]