{-# LANGUAGE NamedFieldPuns #-}
module Compiler.Compiler (astToWasm, makeFuncType, makeExport, compileFunc, compileExpr, buildVarTable, VarTable, buildFuncTable, FuncTable) where

import AST.AST
import qualified Language.Wasm.Structure as Wasm
import qualified Data.Text.Lazy as T -- Idk i think it's like a cast since i returned string but expected text
import Numeric.Natural (Natural) -- WebAssembly needs a absolute val, no potential negatives so no ints pretty much
import qualified Data.Map as Map
type VarTable = Map.Map String Natural -- Maps variable names to their local indices in WebAssembly
type FuncTable = Map.Map String Natural -- Maps function names to their indices in WebAssembly

-- TODO: Will assemble complete module with types, functions, and exports
astToWasm :: Program -> Wasm.Module
astToWasm _program = Wasm.emptyModule -- DONT FORGET TO REMOVE '_' FROM program

-- Creates a var table
buildVarTable :: [Parameter] -> VarTable
buildVarTable params =
  Map.fromList $ zip (map paramName params) [0..] -- zip = ["x", "y", "z"] [0, 1, 2] -> [("x", 0), ("y", 1), ("z", 2)]
--                                          [0..] dynamic inf list

-- Creates a function table
buildFuncTable :: [Function] -> FuncTable
buildFuncTable funcs =
  Map.fromList $ zip (map fName funcs) [0..] -- zip = ["main", "add", "sub"] [0, 1, 2] -> [("main", 0), ("add", 1), ("sub", 2)]

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
compileFunc :: FuncTable -> Natural -> Function -> Wasm.Function
compileFunc funcTable typeIndex (Function {fParams, fBody}) =
  let varTable = buildVarTable fParams
  in Wasm.Function typeIndex [] (compileStatements funcTable varTable fBody)

-- Plural there is a S at the end dummy
compileStatements :: FuncTable -> VarTable -> [Statement] -> [Wasm.Instruction Natural]
compileStatements funcTable varTable = concatMap (compileStatement funcTable varTable)

-- Singular no S
compileStatement :: FuncTable -> VarTable -> Statement -> [Wasm.Instruction Natural]
compileStatement funcTable varTable (Return expr) = compileExpr funcTable varTable expr ++ [Wasm.Return]
compileStatement _ _ _ = error "TODO: other statements"

-- Execution of the expressions, removes previos info from stack like [5, 3] and replaces it with final value like [8] (if it's a addition ofc)
compileExpr :: FuncTable -> VarTable -> Expr -> [Wasm.Instruction Natural]
-- Adds to stack so like [] becomes [5], then if called again [5, 3]
compileExpr _ _ (NumLit (Located _ n)) = [Wasm.I64Const (fromIntegral n)]
-- Boolean literals (true = 1, false = 0 in WebAssembly I32 format)
compileExpr _ _ (BoolLit (Located _ True)) = [Wasm.I32Const 1]
compileExpr _ _ (BoolLit (Located _ False)) = [Wasm.I32Const 0]
-- Adddition
compileExpr funcTable varTable (BinOp Add (Located _ expr1) (Located _ expr2)) =
  compileExpr funcTable varTable expr1 ++ compileExpr funcTable varTable expr2 ++ [Wasm.IBinOp Wasm.BS64 Wasm.IAdd]
-- Substraction
compileExpr funcTable varTable (BinOp Sub (Located _ expr1) (Located _ expr2)) =
  compileExpr funcTable varTable expr1 ++ compileExpr funcTable varTable expr2 ++ [Wasm.IBinOp Wasm.BS64 Wasm.ISub]
-- Multiplication
compileExpr funcTable varTable (BinOp Mul (Located _ expr1) (Located _ expr2)) =
  compileExpr funcTable varTable expr1 ++ compileExpr funcTable varTable expr2 ++ [Wasm.IBinOp Wasm.BS64 Wasm.IMul]
-- Division
compileExpr funcTable varTable (BinOp Div (Located _ expr1) (Located _ expr2)) =
  compileExpr funcTable varTable expr1 ++ compileExpr funcTable varTable expr2 ++ [Wasm.IBinOp Wasm.BS64 Wasm.IDivS]
-- Equality comparison (returns I32: 1 if equal, 0 if not)
compileExpr funcTable varTable (BinOp Eq (Located _ expr1) (Located _ expr2)) =
  compileExpr funcTable varTable expr1 ++ compileExpr funcTable varTable expr2 ++ [Wasm.IRelOp Wasm.BS64 Wasm.IEq]
-- Not equal comparison (returns I32: 1 if different, 0 if same)
compileExpr funcTable varTable (BinOp Neq (Located _ expr1) (Located _ expr2)) =
  compileExpr funcTable varTable expr1 ++ compileExpr funcTable varTable expr2 ++ [Wasm.IRelOp Wasm.BS64 Wasm.INe]
-- Less than comparison (signed, so negatives work correctly)
compileExpr funcTable varTable (BinOp Lt (Located _ expr1) (Located _ expr2)) =
  compileExpr funcTable varTable expr1 ++ compileExpr funcTable varTable expr2 ++ [Wasm.IRelOp Wasm.BS64 Wasm.ILtS]
-- Greater than comparison
compileExpr funcTable varTable (BinOp Gt (Located _ expr1) (Located _ expr2)) =
  compileExpr funcTable varTable expr1 ++ compileExpr funcTable varTable expr2 ++ [Wasm.IRelOp Wasm.BS64 Wasm.IGtS]
-- Less than or equal comparison
compileExpr funcTable varTable (BinOp Le (Located _ expr1) (Located _ expr2)) =
  compileExpr funcTable varTable expr1 ++ compileExpr funcTable varTable expr2 ++ [Wasm.IRelOp Wasm.BS64 Wasm.ILeS]
-- Greater than or equal comparison
compileExpr funcTable varTable (BinOp Ge (Located _ expr1) (Located _ expr2)) =
  compileExpr funcTable varTable expr1 ++ compileExpr funcTable varTable expr2 ++ [Wasm.IRelOp Wasm.BS64 Wasm.IGeS]
-- Logical AND (in c it's &&)
compileExpr funcTable varTable (BinOp And (Located _ expr1) (Located _ expr2)) =
  compileExpr funcTable varTable expr1 ++ compileExpr funcTable varTable expr2 ++ [Wasm.IBinOp Wasm.BS32 Wasm.IAnd]
-- Logical OR (in c it's |)
compileExpr funcTable varTable (BinOp Or (Located _ expr1) (Located _ expr2)) =
  compileExpr funcTable varTable expr1 ++ compileExpr funcTable varTable expr2 ++ [Wasm.IBinOp Wasm.BS32 Wasm.IOr]
-- Variable access (looks up variable name in table and emits GetLocal instruction to read its value from the stack)
compileExpr _ varTable (Var (Located _ name)) =
  case Map.lookup name varTable of
    Just idx -> [Wasm.GetLocal idx]
    Nothing  -> error $ "Variable not found: " ++ name
-- Function calls (compile all arguments and emit Call instruction with function index from table)
compileExpr funcTable varTable (Call (Located _ funcName) args) =
  case Map.lookup funcName funcTable of
    Just funcIdx -> concatMap (compileExpr funcTable varTable) args ++ [Wasm.Call funcIdx]
    Nothing -> error $ "Function not found: " ++ funcName