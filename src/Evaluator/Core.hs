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

import Types
import Environment
import Evaluator.Special
import Evaluator.Apply

-- The main evaluator
eval :: Env -> LispVal -> Either String LispVal
eval _ val@(Number _) = Right val
eval _ val@(Bool _) = Right val

-- Case 1: (atoms)
eval env (Atom name) =
  lookupVar name env

-- Case 2: Special forms
eval env (List (Atom "define" : rest)) = evalDefine eval env rest
eval env (List (Atom "if" : rest)) = evalIf eval env rest
eval env (List (Atom "lambda" : rest)) = evalLambda env rest

-- Case 3: Function calls
eval env (List (fnExpr : argExprs)) = do
  function <- eval env fnExpr
  args <- mapM (eval env) argExprs
  apply eval function args

-- Anything else is just wrong
eval _ bad =
  Left $ "*** ERROR: cannot evaluate " ++ show bad