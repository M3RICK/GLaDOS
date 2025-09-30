{-# LANGUAGE NamedFieldPuns #-}
module Compiler.Compiler (astToWasm, makeFuncType, makeExport ,compileFunc) where

import AST.AST
import qualified Language.Wasm.Structure as Wasm
import qualified Data.Text.Lazy as T -- Idk i think it's like a cast since i returned string but expected text
import Numeric.Natural (Natural) -- WebAssembly needs a absolute val, no potential negatives so no ints pretty much

-- TODO: Will assemble complete module with types, functions, and exports
astToWasm :: Program -> Wasm.Module
astToWasm _program = Wasm.emptyModule -- DONT FORGET TO REMOVE '_' FROM program

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

-- Self explanatory
convertType :: Type -> Wasm.ValueType
convertType TypeInt  = Wasm.I64
convertType TypeBool = Wasm.I32
convertType TypeVoid = error "TypeVoid shouldn't be converted to ValueType"

-- This buids the "list" and exec order, so like index 0 is ADD
makeExport :: Natural -> Function -> Wasm.Export
makeExport funcIndex (Function {fName}) = 
  Wasm.Export (T.pack fName) (Wasm.ExportFunc funcIndex)

-- Takes an (AST Function) turns it into a (Wasm.Function). typeIndex = type signature, [] = variables (will be added later), finally the wasm instructions themselfes
compileFunc :: Natural -> Function -> Wasm.Function
compileFunc typeIndex (Function {fBody}) =
  Wasm.Function typeIndex [] (compileStatements fBody)

-- Plural there is a S at the end dummy
compileStatements :: [Statement] -> [Wasm.Instruction Natural]
compileStatements = concatMap compileStatement

-- Singular no S
compileStatement :: Statement -> [Wasm.Instruction Natural]
compileStatement (Return expr) = compileExpr expr ++ [Wasm.Return]
compileStatement _ = error "TODO: other statements"

-- Execution of the expressions, removes previos info from stack like [5, 3] and replaces it with final value like [8] (if it's a addition ofc)
compileExpr :: Expr -> [Wasm.Instruction Natural]
-- Adds to stack so like [] becomes [5], then if called again [5, 3]
compileExpr (NumLit n) = [Wasm.I64Const (fromIntegral n)]
-- Adddition
compileExpr (BinOp Add expr1 expr2) = 
  compileExpr expr1 ++ compileExpr expr2 ++ [Wasm.IBinOp Wasm.BS64 Wasm.IAdd]
-- Substraction
compileExpr (BinOp Sub expr1 expr2) = 
  compileExpr expr1 ++ compileExpr expr2 ++ [Wasm.IBinOp Wasm.BS64 Wasm.ISub]
-- Multiplication
compileExpr (BinOp Mul expr1 expr2) = 
  compileExpr expr1 ++ compileExpr expr2 ++ [Wasm.IBinOp Wasm.BS64 Wasm.IMul]
-- division
compileExpr (BinOp Div expr1 expr2) = 
  compileExpr expr1 ++ compileExpr expr2 ++ [Wasm.IBinOp Wasm.BS64 Wasm.IDivS]
-- Dunno yet
compileExpr _ = error "TODO: other expressions"