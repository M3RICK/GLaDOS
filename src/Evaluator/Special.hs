module Evaluator.Special(evalDefine, evalIf, evalLambda) where

import Types
import Environment
import Evaluator.Helpers

-- Handle the (define ...) special form
-- Example: (define foo 42)
evalDefine::(Env -> LispVal -> Either String LispVal)
           -> Env -> [LispVal] -> Either String LispVal

-- Case 1: variable definition
evalDefine eval env [Atom var, expr] =
    case eval env expr of
        Right val -> Right (Function [] (Atom var) (defineVar var val env))
        Left err  -> Left err

evalDefine _ _ other =
    Left $ "*** ERROR: malformed define: " ++ show other


-- Handle the (if ...) special form
-- Example: (if #t 1 2)
evalIf::(Env -> LispVal -> Either String LispVal)
       -> Env -> [LispVal] -> Either String LispVal

-- Well-formed if with 3 expressions
evalIf eval env [condExpr, thenExpr, elseExpr] =
    case eval env condExpr of
        Right condVal ->
            if isTrue condVal
                then eval env thenExpr
                else eval env elseExpr
        Left err -> Left err

evalIf _ _ other =
    Left $ "*** ERROR: malformed if: " ++ show other


-- (lambda (x y) (+ x y))
evalLambda::Env -> [LispVal] -> Either String LispVal

-- list of params + body
evalLambda env [List params, body] =
    let paramNames = [ name | Atom name <- params ]
    in Right (Function paramNames body env)


evalLambda _ other =
    Left $ "*** ERROR: malformed lambda: " ++ show other
