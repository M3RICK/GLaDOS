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
    convertParams (Parameter paramType _ : rest) =
      convertType paramType : convertParams rest

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
compileExpr (NumLit (Located _ n)) = [Wasm.I64Const (fromIntegral n)]
-- Boolean literals (true = 1, false = 0 in WebAssembly I32 format)
compileExpr (BoolLit (Located _ True)) = [Wasm.I32Const 1]
compileExpr (BoolLit (Located _ False)) = [Wasm.I32Const 0]
-- Adddition
compileExpr (BinOp Add (Located _ expr1) (Located _ expr2)) =
  compileExpr expr1 ++ compileExpr expr2 ++ [Wasm.IBinOp Wasm.BS64 Wasm.IAdd]
-- Substraction
compileExpr (BinOp Sub (Located _ expr1) (Located _ expr2)) =
  compileExpr expr1 ++ compileExpr expr2 ++ [Wasm.IBinOp Wasm.BS64 Wasm.ISub]
-- Multiplication
compileExpr (BinOp Mul (Located _ expr1) (Located _ expr2)) =
  compileExpr expr1 ++ compileExpr expr2 ++ [Wasm.IBinOp Wasm.BS64 Wasm.IMul]
-- Division
compileExpr (BinOp Div (Located _ expr1) (Located _ expr2)) =
  compileExpr expr1 ++ compileExpr expr2 ++ [Wasm.IBinOp Wasm.BS64 Wasm.IDivS]
-- Equality comparison (returns I32: 1 if equal, 0 if not)
compileExpr (BinOp Eq (Located _ expr1) (Located _ expr2)) =
  compileExpr expr1 ++ compileExpr expr2 ++ [Wasm.IRelOp Wasm.BS64 Wasm.IEq]
-- Not equal comparison (returns I32: 1 if different, 0 if same)
compileExpr (BinOp Neq (Located _ expr1) (Located _ expr2)) =
  compileExpr expr1 ++ compileExpr expr2 ++ [Wasm.IRelOp Wasm.BS64 Wasm.INe]
-- Less than comparison (signed, so negatives work correctly)
compileExpr (BinOp Lt (Located _ expr1) (Located _ expr2)) =
  compileExpr expr1 ++ compileExpr expr2 ++ [Wasm.IRelOp Wasm.BS64 Wasm.ILtS]
-- Greater than comparison
compileExpr (BinOp Gt (Located _ expr1) (Located _ expr2)) =
  compileExpr expr1 ++ compileExpr expr2 ++ [Wasm.IRelOp Wasm.BS64 Wasm.IGtS]
-- Less than or equal comparison
compileExpr (BinOp Le (Located _ expr1) (Located _ expr2)) =
  compileExpr expr1 ++ compileExpr expr2 ++ [Wasm.IRelOp Wasm.BS64 Wasm.ILeS]
-- Greater than or equal comparison
compileExpr (BinOp Ge (Located _ expr1) (Located _ expr2)) =
  compileExpr expr1 ++ compileExpr expr2 ++ [Wasm.IRelOp Wasm.BS64 Wasm.IGeS]
-- Logical AND (uses bitwise AND on I32 booleans, 1 & 1 = 1, anything else = 0)
compileExpr (BinOp And (Located _ expr1) (Located _ expr2)) =
  compileExpr expr1 ++ compileExpr expr2 ++ [Wasm.IBinOp Wasm.BS32 Wasm.IAnd]
-- Logical OR (uses bitwise OR on I32 booleans, 0 | 0 = 0, anything else = 1)
compileExpr (BinOp Or (Located _ expr1) (Located _ expr2)) =
  compileExpr expr1 ++ compileExpr expr2 ++ [Wasm.IBinOp Wasm.BS32 Wasm.IOr]
-- TODO: Variable access (needs a symbol table to map variable names to local indices)
-- TODO: Function calls (needs a function table to map function names to indices, then compile all args and emit Call instruction)