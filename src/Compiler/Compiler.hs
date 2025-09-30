{-# LANGUAGE NamedFieldPuns #-}
module Compiler.Compiler where

import AST.AST
import qualified Language.Wasm.Structure as Wasm
import Language.Wasm.Structure (FuncType(..), ValueType(..))
import qualified Data.Text.Lazy as T -- Idk i think it's like a cast since i returned string but expected text
import Numeric.Natural (Natural) -- WebAssembly needs a absolute val, no potential negatives so no ints pretty much

-- Placeholder creates a empty module for now
astToWasm :: Program -> Wasm.Module
astToWasm program =
    Wasm.emptyModule

-- Basically formats each function to wasm 
makeFuncType :: Function -> Wasm.FuncType
makeFuncType (Function {fType, fParams}) = 
  Wasm.FuncType paramTypes returnTypes
  where
    paramTypes = convertParams fParams

    convertParams [] = []
    convertParams (Parameter pType _ : rest) = 
      convertType pType : convertParams rest

    returnTypes = case fType of
      TypeVoid -> []
      _        -> [convertType fType]

convertType :: Type -> Wasm.ValueType
convertType TypeInt  = Wasm.I64
convertType TypeBool = Wasm.I32
convertType TypeVoid = error "TypeVoid shouldn't be converted to ValueType"

-- This buids the "list" and exec order, so like index 0 is ADD
makeExport :: Natural -> Function -> Wasm.Export
makeExport funcIndex (Function {fName}) = 
  Wasm.Export (T.pack fName) (Wasm.ExportFunc funcIndex)

compileFunc :: Natural -> Function -> Wasm.Function
compileFunc typeIndex (Function {fParams, fBody}) = 
  Wasm.Function
    { funcType = typeIndex
    , funcLocals = []
    , funcBody = compileStatements fBody
    }

compileStatements :: [Statement] -> [Wasm.Instruction]
compileStatements = concatMap compileStatement

compileStatement :: Statement -> [Wasm.Instruction]
compileStatement (Return expr) = compileExpr expr ++ [Wasm.Return]
compileStatement _ = error "TODO: other statements"

-- Execution of the expressions, removes previos info from stack like [5, 3] and replaces it with final value like [8] (if it's a addition ofc)
compileExpr :: Expr -> [Wasm.Instruction]
-- Adds to stack so like [] becomes [5], then if called again [5, 3]
compileExpr (NumLit n) = [Wasm.I64Const (fromIntegral n)]
-- Adddition
compileExpr (BinOp Add expr1 expr2) = 
  compileExpr expr1 ++ compileExpr expr2 ++ [Wasm.I64Add]
-- Substraction
compileExpr (BinOp Sub expr1 expr2) = 
  compileExpr expr1 ++ compileExpr expr2 ++ [Wasm.I64Sub]
-- Multiplication
compileExpr (BinOp Mul expr1 expr2) = 
  compileExpr expr1 ++ compileExpr expr2 ++ [Wasm.I64Mul]
-- division
compileExpr (BinOp Div expr1 expr2) = 
  compileExpr expr1 ++ compileExpr expr2 ++ [Wasm.I64DivS]
-- Dunno yet
compileExpr _ = error "TODO: other expressions"