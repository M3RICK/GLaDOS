module Evaluator.Core (eval) where

import Types
import Environment
import Evaluator.Special
import Evaluator.Apply

eval::Env -> LispVal -> Either String (Env, LispVal)

-- helper: eval list of args while threading env
evalArgs :: Env -> [LispVal] -> Either String (Env, [LispVal])
evalArgs env [] = Right (env, [])
evalArgs env (x:xs) = do
    (env1, val)     <- eval env x
    (env2, rest)    <- evalArgs env1 xs
    Right (env2, val:rest)

-- Base cases
eval env val@(Number _) = Right (env, val)
eval env val@(Bool _)   = Right (env, val)
eval env (Atom name) =
    case lookupVar name env of
        Right v -> Right (env, v)
        Left e  -> Left e

-- Special forms
eval env (List (Atom "define" : rest)) = evalDefine eval env rest
eval env (List (Atom "if"     : rest)) = evalIf eval env rest
eval env (List (Atom "lambda" : rest)) = evalLambda env rest

-- Function calls
eval env (List (fnExpr : argExprs)) = do
    (env1, fn)   <- eval env fnExpr
    (env2, args) <- evalArgs env1 argExprs
    result       <- apply eval fn args
    Right (env2, result)

eval _ bad = Left $ "*** ERROR: cannot evaluate " ++ show bad
