module Evaluator.Special (evalDefine, evalIf, evalLambda) where

import Types
import Environment
import Evaluator.Helpers

-- (define var expr) or (define (fname args...) body)
evalDefine::(Env -> LispVal -> Either String (Env, LispVal))
           -> Env -> [LispVal] -> Either String (Env, LispVal)

-- Case 1: variable define
evalDefine eval env [Atom var, expr] = do
    (env1, val) <- eval env expr
    let newEnv = defineVar var val env1
    Right (newEnv, val)

-- Case 2: function definition
evalDefine _eval env [List (Atom fname : params), body] =
    let paramNames = [ name | Atom name <- params ]
        recEnv = defineVar fname fn env
        fn = Function paramNames body recEnv
    in Right (recEnv, Atom fname)

-- Invalid define
evalDefine _ _ other =
    Left $ "*** ERROR: malformed define: " ++ show other


-- (if cond then else)
evalIf::(Env -> LispVal -> Either String (Env, LispVal))
       -> Env -> [LispVal] -> Either String (Env, LispVal)
evalIf eval env [condExpr, thenExpr, elseExpr] = do
    (_, condVal) <- eval env condExpr
    if isTrue condVal
       then eval env thenExpr
       else eval env elseExpr
evalIf _ _ other =
    Left $ "*** ERROR: malformed if: " ++ show other


-- (lambda (args...) body)
evalLambda :: Env -> [LispVal] -> Either String (Env, LispVal)
evalLambda env [List params, body] =
    let paramNames = [ name | Atom name <- params ]
        fn = Function paramNames body env
    in Right (env, fn)
evalLambda _ other =
    Left $ "*** ERROR: malformed lambda: " ++ show other
