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
