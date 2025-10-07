module Security.StatementCheckers where

import Security.Types
import Security.Environment
import Security.ExprChecker
import AST.AST
import AST.Helpers (getExprPos)

checkDecl :: CheckEnv -> Type -> String -> Maybe Expr -> (CheckEnv, [TypeError])
checkDecl env typ name maybeExpr =
  let errors = maybe [] (\expr -> checkExpr env expr typ) maybeExpr
      newEnv = addVar name typ env
      finalEnv = maybe newEnv (const $ markInitialized name newEnv) maybeExpr
  in (finalEnv, errors)

checkAssign :: CheckEnv -> String -> Expr -> (CheckEnv, [TypeError])
checkAssign env name expr =
  case lookupVar name env of
    Nothing -> (env, [UndefinedVar name (getExprPos expr)])
    Just expectedType ->
      let errors = checkExpr env expr expectedType
          newEnv = markInitialized name env
      in (newEnv, errors)

checkReturn :: CheckEnv -> Expr -> (CheckEnv, [TypeError])
checkReturn env expr =
  case currentReturnType env of
    Nothing -> (env, [makeReturnError expr "return outside function"])
    Just TypeVoid -> (env, [makeReturnError expr "void function cannot return"])
    Just retType -> (env, checkExpr env expr retType)

checkIf :: CheckEnv -> Expr -> [Statement] -> Maybe [Statement] -> (CheckEnv, [TypeError])
checkIf env cond thenBody elseBody =
  let condErrors = checkExpr env cond TypeBool
      thenErrors = checkStatements env thenBody
      elseErrors = maybe [] (checkStatements env) elseBody
  in (env, condErrors ++ thenErrors ++ elseErrors)

checkWhile :: CheckEnv -> Expr -> [Statement] -> (CheckEnv, [TypeError])
checkWhile env cond body =
  let condErrors = checkExpr env cond TypeBool
      bodyErrors = checkStatements env body
  in (env, condErrors ++ bodyErrors)

checkExprStmt :: CheckEnv -> Expr -> (CheckEnv, [TypeError])
checkExprStmt env expr = (env, checkExprValid env expr)

makeReturnError :: Expr -> String -> TypeError
makeReturnError expr msg =
  TypeMismatch TypeVoid TypeInt (getExprPos expr) msg

checkStatements :: CheckEnv -> [Statement] -> [TypeError]
checkStatements _ [] = []
checkStatements env (stmt:rest) =
  let (newEnv, errors) = checkStatement env stmt
  in errors ++ checkStatements newEnv rest

checkStatement :: CheckEnv -> Statement -> (CheckEnv, [TypeError])
checkStatement env stmt = case stmt of
  Decl t n e -> checkDecl env t n e
  Assign n e -> checkAssign env n e
  Return e -> checkReturn env e
  If c t e -> checkIf env c t e
  While c b -> checkWhile env c b
  ExprStmt e -> checkExprStmt env e
