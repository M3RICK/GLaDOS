module Compiler.Compiler where
import AST.AST (Program)
import qualified Language.Wasm.Structure as Wasm

astToWasm :: Program -> Wasm.Module
astToWasm program =
    Wasm.emptyModule

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