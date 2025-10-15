module Security.ErrorFormat
  ( formatError
  , formatTypeErrors
  ) where

import Security.Types
import AST.AST
import Text.Megaparsec.Pos

-- Select les formatted error types
formatError :: TypeError -> String
formatError (UndefinedVar name p) =
  formatUndefinedVar name p
formatError (UndefinedFunc name p) =
  formatUndefinedFunc name p
formatError (TypeMismatch expectedType actualType p ctx) =
  formatTypeMismatch expectedType actualType p ctx
formatError (AlreadyDefined name p) =
  formatAlreadyDefined name p
formatError (WrongArgCount name expectedType actualType p) =
  formatWrongArgCount name expectedType actualType p
formatError (ReturnTypeMismatch expectedType actualType p) =
  formatReturnTypeMismatch expectedType actualType p
formatError (UninitializedVar name p) =
  formatUninitializedVar name p
formatError (MissingReturn name) =
  formatMissingReturn name
formatError (DivisionByZero p) =
  "Division by zero at " ++ show p

-- Message error formatters
formatUndefinedVar :: String -> SourcePos -> String
formatUndefinedVar name p =
  "Undefined variable '" ++ name ++ "' at " ++ show p

formatUndefinedFunc :: String -> SourcePos -> String
formatUndefinedFunc name p =
  "Undefined function '" ++ name ++ "' at " ++ show p

formatTypeMismatch :: Type -> Type -> SourcePos -> String -> String
formatTypeMismatch expectedType actualType p ctx =
  "Type error " ++ ctx ++ " at " ++ show p ++
  ": expected " ++ showType expectedType ++ " but got " ++ showType actualType

formatAlreadyDefined :: String -> SourcePos -> String
formatAlreadyDefined name p =
  "Variable '" ++ name ++ "' already defined at " ++ show p

formatWrongArgCount :: String -> Int -> Int -> SourcePos -> String
formatWrongArgCount name expectedType actualType p =
  "Function '" ++ name ++ "' expects " ++ show expectedType ++
  " arguments but got " ++ show actualType ++ " at " ++ show p

formatReturnTypeMismatch :: Type -> Type -> SourcePos -> String
formatReturnTypeMismatch expectedType actualType p =
  "Return type mismatch at " ++ show p ++
  ": expected " ++ showType expectedType ++ " but got " ++ showType actualType

formatUninitializedVar :: String -> SourcePos -> String
formatUninitializedVar name p =
  "Variable '" ++ name ++ "' used before initialization at " ++ show p

formatMissingReturn :: String -> String
formatMissingReturn name =
  "Function '" ++ name ++ "' must return a value on all code paths"


-- On affiche ca pour que ce soit joli
showType :: Type -> String
showType TypeInt = "int"
showType TypeFloat = "float"
showType TypeBool = "bool"
showType TypeVoid = "void"
showType TypeInfer = "var"

-- Format multiple type errors
formatTypeErrors :: [TypeError] -> String
formatTypeErrors [] = "Unknown type error"
formatTypeErrors errors =
  unlines $ map formatError errors
