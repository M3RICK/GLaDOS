module Security.ErrorFormat where

import Security.Types
import AST.AST
import Text.Megaparsec.Pos

-- Affiche Format type error for display
showTypeError :: TypeError -> String
showTypeError = formatError

-- Select les formatted error types
formatError :: TypeError -> String
formatError (UndefinedVar name pos) =
  formatUndefinedVar name pos
formatError (UndefinedFunc name pos) =
  formatUndefinedFunc name pos
formatError (TypeMismatch exp got pos ctx) =
  formatTypeMismatch exp got pos ctx
formatError (AlreadyDefined name pos) =
  formatAlreadyDefined name pos
formatError (WrongArgCount name exp got pos) =
  formatWrongArgCount name exp got pos
formatError (ReturnTypeMismatch exp got pos) =
  formatReturnTypeMismatch exp got pos
formatError (UninitializedVar name pos) =
  formatUninitializedVar name pos

-- Message error formatters
formatUndefinedVar :: String -> SourcePos -> String
formatUndefinedVar name pos =
  "Undefined variable '" ++ name ++ "' at " ++ show pos

formatUndefinedFunc :: String -> SourcePos -> String
formatUndefinedFunc name pos =
  "Undefined function '" ++ name ++ "' at " ++ show pos

formatTypeMismatch :: Type -> Type -> SourcePos -> String -> String
formatTypeMismatch exp got pos ctx =
  "Type error " ++ ctx ++ " at " ++ show pos ++
  ": expected " ++ showType exp ++ " but got " ++ showType got

formatAlreadyDefined :: String -> SourcePos -> String
formatAlreadyDefined name pos =
  "Variable '" ++ name ++ "' already defined at " ++ show pos

formatWrongArgCount :: String -> Int -> Int -> SourcePos -> String
formatWrongArgCount name exp got pos =
  "Function '" ++ name ++ "' expects " ++ show exp ++
  " arguments but got " ++ show got ++ " at " ++ show pos

formatReturnTypeMismatch :: Type -> Type -> SourcePos -> String
formatReturnTypeMismatch exp got pos =
  "Return type mismatch at " ++ show pos ++
  ": expected " ++ showType exp ++ " but got " ++ showType got

formatUninitializedVar :: String -> SourcePos -> String
formatUninitializedVar name pos =
  "Variable '" ++ name ++ "' used before initialization at " ++ show pos

-- On affiche ca pour que ce soit joli
showType :: Type -> String
showType TypeInt = "int"
showType TypeBool = "bool"
showType TypeVoid = "void"
