module Security.OperatorChecks
  ( checkBinOp
  , checkUnOp
  ) where

import Security.Types
import AST.AST
import AST.Helpers (getExprPos)

checkBinOp :: CheckEnv -> Op -> Expr -> Expr -> (Expr -> Either TypeError Type) -> Either TypeError Type
checkBinOp env op e1 e2 getType =
  getOperatorChecker op env e1 e2 getType

checkUnOp :: CheckEnv -> UnOp -> Expr -> (Expr -> Either TypeError Type) -> Either TypeError Type
checkUnOp env op e getType =
  getUnaryOperatorChecker op env e getType

getOperatorChecker :: Op -> CheckEnv -> Expr -> Expr -> (Expr -> Either TypeError Type) -> Either TypeError Type
getOperatorChecker op = case op of
  Add -> numericOp
  Sub -> numericOp
  Mul -> numericOp
  Div -> divisionOp
  Lt  -> comparisonOp
  Gt  -> comparisonOp
  Le  -> comparisonOp
  Ge  -> comparisonOp
  Eq  -> equalityOp TypeBool
  Neq -> equalityOp TypeBool
  And -> logicalOp TypeBool
  Or  -> logicalOp TypeBool

getUnaryOperatorChecker :: UnOp -> CheckEnv -> Expr -> (Expr -> Either TypeError Type) -> Either TypeError Type
getUnaryOperatorChecker op = case op of
  Neg -> negationOp
  Not -> notOp

-- Numeric operations work on both Int and Float, return same type
numericOp :: CheckEnv -> Expr -> Expr -> (Expr -> Either TypeError Type) -> Either TypeError Type
numericOp _ e1 e2 getType = do
  t1 <- getType e1
  t2 <- getType e2
  case (t1, t2) of
    (TypeInt, TypeInt) -> return TypeInt
    (TypeFloat, TypeFloat) -> return TypeFloat
    _ -> Left (TypeMismatch t1 t2 (getExprPos e2) "same type sinon ca marche pas")

-- Comparison operations work on both Int and Float, return Bool
comparisonOp :: CheckEnv -> Expr -> Expr -> (Expr -> Either TypeError Type) -> Either TypeError Type
comparisonOp _ e1 e2 getType = do
  t1 <- getType e1
  t2 <- getType e2
  case (t1, t2) of
    (TypeInt, TypeInt) -> return TypeBool
    (TypeFloat, TypeFloat) -> return TypeBool
    _ -> Left (TypeMismatch t1 t2 (getExprPos e2) "on compare pas des patates et des tomates")

equalityOp :: Type -> CheckEnv -> Expr -> Expr -> (Expr -> Either TypeError Type) -> Either TypeError Type
equalityOp resultType _ e1 e2 getType = do
  t1 <- getType e1
  t2 <- getType e2
  if t1 == t2
    then return resultType
    else Left (TypeMismatch t1 t2 (getExprPos e2) "in equality")

logicalOp :: Type -> CheckEnv -> Expr -> Expr -> (Expr -> Either TypeError Type) -> Either TypeError Type
logicalOp resultType _ e1 e2 getType = do
  requireType TypeBool e1 getType "in logical operation"
  requireType TypeBool e2 getType "in logical operation"
  return resultType

requireType :: Type -> Expr -> (Expr -> Either TypeError Type) -> String -> Either TypeError ()
requireType expectedType expr getType ctx = do
  actual <- getType expr
  if actual == expectedType
    then Right ()
    else Left (TypeMismatch expectedType actual (getExprPos expr) ctx)

-- Check if expression is literal zero
checkNotZero :: Expr -> Either TypeError ()
checkNotZero (NumLit (Located p 0)) = Left (DivisionByZero p)
checkNotZero (FloatLit (Located p 0.0)) = Left (DivisionByZero p)
checkNotZero _ = Right ()

divisionOp :: CheckEnv -> Expr -> Expr -> (Expr -> Either TypeError Type) -> Either TypeError Type
divisionOp _ e1 e2 getType = do
  t1 <- getType e1
  t2 <- getType e2
  checkNotZero e2
  case (t1, t2) of
    (TypeInt, TypeInt) -> return TypeInt
    (TypeFloat, TypeFloat) -> return TypeFloat
    _ -> Left (TypeMismatch t1 t2 (getExprPos e2) "in division les deux oper ont besoins d etre Ints ou floats")

-- Negation operation works on both Int and Float, returns same type
negationOp :: CheckEnv -> Expr -> (Expr -> Either TypeError Type) -> Either TypeError Type
negationOp _ e getType = do
  t <- getType e
  case t of
    TypeInt -> return TypeInt
    TypeFloat -> return TypeFloat
    _ -> Left (TypeMismatch TypeInt t (getExprPos e) "in negation teuteuteu ca a besoin d etre un int ou un float")

-- Logical not operation works on Bool, returns Bool
notOp :: CheckEnv -> Expr -> (Expr -> Either TypeError Type) -> Either TypeError Type
notOp _ e getType = do
  requireType TypeBool e getType "in logical not"
  return TypeBool
