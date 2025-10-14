module Security.ExprChecker where

import Text.Megaparsec.Pos (SourcePos)
import Security.Types
import Security.Environment
import Security.OperatorChecks
import AST.AST
import AST.Helpers (getExprPos)

-- Check expression has expected type
checkExpr :: CheckEnv -> Expr -> Type -> [TypeError]
checkExpr env expr expectedType =
  either (:[]) (validateType expectedType expr) (getExprType env expr)

checkExprValid :: CheckEnv -> Expr -> [TypeError]
checkExprValid env expr =
  either (:[]) (const []) (getExprType env expr)

getExprType :: CheckEnv -> Expr -> Either TypeError Type
getExprType env expr = case expr of
  BoolLit _  -> Right TypeBool
  NumLit _   -> Right TypeInt
  FloatLit _ -> Right TypeFloat
  Var loc    -> checkVariable env loc
  BinOp op (Located _ e1) (Located _ e2) ->
    checkBinOp env op e1 e2 (getExprType env)
  UnOp op (Located _ e) ->
    checkUnOp env op e (getExprType env)
  Call loc args -> checkFunctionCall env loc args

-- Check reference
checkVariable :: CheckEnv -> Located String -> Either TypeError Type
checkVariable env (Located pos name) =
  case lookupVar name env of
    Nothing -> Left $ UndefinedVar name pos
    Just typ -> validateInitialized env name pos typ

-- Variable is initialized
validateInitialized :: CheckEnv -> String -> SourcePos -> Type -> Either TypeError Type
validateInitialized env name pos typ
  | isInitialized name env = Right typ
  | otherwise = Left $ UninitializedVar name pos

checkFunctionCall :: CheckEnv -> Located String -> [Expr] -> Either TypeError Type
checkFunctionCall env (Located pos name) args =
  case lookupFunc name env of
    Nothing -> Left (UndefinedFunc name pos)
    Just (retType, paramTypes) ->
      validateCall env name pos args paramTypes retType

validateCall :: CheckEnv -> String -> SourcePos -> [Expr] -> [Type] -> Type -> Either TypeError Type
validateCall env name pos args paramTypes retType =
  case validateArgs env name pos args paramTypes of
    Left err -> Left err
    Right _ -> Right retType

validateArgs :: CheckEnv -> String -> SourcePos -> [Expr] -> [Type] -> Either TypeError ()
validateArgs env name pos args paramTypes
  | wrongCount = Left $ WrongArgCount name (length paramTypes) (length args) pos
  | otherwise = checkArgTypes env args paramTypes
  where wrongCount = length args /= length paramTypes

checkArgTypes :: CheckEnv -> [Expr] -> [Type] -> Either TypeError ()
checkArgTypes env args paramTypes =
  let errors = concat $ zipWith (checkExpr env) args paramTypes
  in if null errors
        then Right ()
        else Left (head errors)

validateType :: Type -> Expr -> Type -> [TypeError]
validateType expected expr actual =
  if actual == expected
    then []
    else [TypeMismatch expected actual (getExprPos expr) ""]
