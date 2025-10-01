module Security.OperatorChecks where

import Security.Types
import AST.AST
import AST.Helpers (getExprPos)

checkBinOp :: CheckEnv -> Op -> Expr -> Expr -> (Expr -> Either TypeError Type) -> Either TypeError Type
checkBinOp env op e1 e2 getType =
  getOperatorChecker op env e1 e2 getType

getOperatorChecker :: Op -> CheckEnv -> Expr -> Expr -> (Expr -> Either TypeError Type) -> Either TypeError Type
getOperatorChecker op = case op of
  Add -> arithmeticOp TypeInt
  Sub -> arithmeticOp TypeInt
  Mul -> arithmeticOp TypeInt
  Div -> arithmeticOp TypeInt
  Lt  -> comparisonOp TypeBool
  Gt  -> comparisonOp TypeBool
  Le  -> comparisonOp TypeBool
  Ge  -> comparisonOp TypeBool
  Eq  -> equalityOp TypeBool
  Neq -> equalityOp TypeBool
  And -> logicalOp TypeBool
  Or  -> logicalOp TypeBool

arithmeticOp :: Type -> CheckEnv -> Expr -> Expr -> (Expr -> Either TypeError Type) -> Either TypeError Type
arithmeticOp resultType env e1 e2 getType = do
  requireType TypeInt e1 getType "in arithmetic"
  requireType TypeInt e2 getType "in arithmetic"
  return resultType

comparisonOp :: Type -> CheckEnv -> Expr -> Expr -> (Expr -> Either TypeError Type) -> Either TypeError Type
comparisonOp resultType env e1 e2 getType = do
  requireType TypeInt e1 getType "in comparison"
  requireType TypeInt e2 getType "in comparison"
  return resultType

equalityOp :: Type -> CheckEnv -> Expr -> Expr -> (Expr -> Either TypeError Type) -> Either TypeError Type
equalityOp resultType env e1 e2 getType = do
  t1 <- getType e1
  t2 <- getType e2
  if t1 == t2
    then return resultType
    else Left (TypeMismatch t1 t2 (getExprPos e2) "in equality")

logicalOp :: Type -> CheckEnv -> Expr -> Expr -> (Expr -> Either TypeError Type) -> Either TypeError Type
logicalOp resultType env e1 e2 getType = do
  requireType TypeBool e1 getType "in logical operation"
  requireType TypeBool e2 getType "in logical operation"
  return resultType

requireType :: Type -> Expr -> (Expr -> Either TypeError Type) -> String -> Either TypeError ()
requireType expected expr getType context = do
  actual <- getType expr
  if actual == expected
    then Right ()
    else Left (TypeMismatch expected actual (getExprPos expr) context)
