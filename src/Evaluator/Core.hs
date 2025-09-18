<<<<<<< HEAD
-- module Evaluator.Core(eval) where

-- import Types
-- import Environment
-- import Evaluator.Special
-- import Evaluator.Apply

-- -- The main man
-- eval::Env -> LispVal -> Either String LispVal

-- eval _ val@(Number _) = Right val
-- eval _ val@(Bool _)   = Right val

-- -- Case 1: (atoms)
-- eval env (Atom name) =
--     lookupVar name env

-- -- Case 2: Special
-- eval env (List (Atom "define" : rest)) = evalDefine eval env rest
-- eval env (List (Atom "if"     : rest)) = evalIf eval env rest
-- eval env (List (Atom "lambda" : rest)) = evalLambda env rest

-- -- Case 3: Function calls
-- eval env (List (fnExpr : argExprs)) = do
--     function <- eval env fnExpr
--     args <- mapM (eval env) argExprs
--     apply eval function args

-- -- Anything else is just wrong
-- eval _ bad =
--     Left $ "*** ERROR: cannot evaluate " ++ show bad




module Evaluator.Core(eval) where
=======
module Evaluator.Core (eval) where
>>>>>>> dev_evaluator

import Types
import Environment
import Evaluator.Special
import Evaluator.Apply

<<<<<<< HEAD
-- The main evaluator
eval :: Env -> LispVal -> Either String LispVal
eval _ val@(Number _) = Right val
eval _ val@(Bool _) = Right val
=======
eval::Env -> LispVal -> Either String (Env, LispVal)

-- helper: eval list of args while threading env
evalArgs :: Env -> [LispVal] -> Either String (Env, [LispVal])
evalArgs env [] = Right (env, [])
evalArgs env (x:xs) = do
    (env1, val)     <- eval env x
    (env2, rest)    <- evalArgs env1 xs
    Right (env2, val:rest)
>>>>>>> dev_evaluator

-- Base cases
eval env val@(Number _) = Right (env, val)
eval env val@(Bool _)   = Right (env, val)
eval env (Atom name) =
<<<<<<< HEAD
  lookupVar name env

-- Case 2: Special forms
=======
    case lookupVar name env of
        Right v -> Right (env, v)
        Left e  -> Left e

-- Special forms
>>>>>>> dev_evaluator
eval env (List (Atom "define" : rest)) = evalDefine eval env rest
eval env (List (Atom "if" : rest)) = evalIf eval env rest
eval env (List (Atom "lambda" : rest)) = evalLambda env rest

-- Function calls
eval env (List (fnExpr : argExprs)) = do
<<<<<<< HEAD
  function <- eval env fnExpr
  args <- mapM (eval env) argExprs
  apply eval function args

-- Anything else is just wrong
eval _ bad =
  Left $ "*** ERROR: cannot evaluate " ++ show bad
=======
    (env1, fn)   <- eval env fnExpr
    (env2, args) <- evalArgs env1 argExprs
    result       <- apply eval fn args
    Right (env2, result)

eval _ bad = Left $ "*** ERROR: cannot evaluate " ++ show bad
>>>>>>> dev_evaluator
