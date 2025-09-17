-- module Evaluator.Apply (apply) where

-- import Types
-- import Environment

-- apply::(Env -> LispVal -> Either String LispVal)
--       -> LispVal -> [LispVal] -> Either String LispVal

-- -- Case 1: like (+, -, eq?, <, etc.)
-- apply _ (Builtin f) args =
--     f args

-- -- Case 2: (lambda)
-- apply eval (Function params body closureEnv) args
--     | length params /= length args =
--         Left $ "*** ERROR: wrong number of arguments. Expected "
--              ++ show (length params) ++ ", got " ++ show (length args)
--     | otherwise =
--         let extendedEnv = foldl
--                             (\env (p,a) -> defineVar p a env)
--                             closureEnv
--                             (zip params args)
--         in eval extendedEnv body

-- apply _ notAFunction _ =
--     Left $ "*** ERROR: tried to apply non-function: " ++ show notAFunction



module Evaluator.Apply (apply) where

import Types
import Environment

apply :: (Env -> LispVal -> Either String LispVal)
      -> LispVal -> [LispVal] -> Either String LispVal

-- Case 1: like (+, -, eq?, <, etc.)
apply _ (Builtin f) args =
  f args

-- Case 2: (lambda)
apply eval (Function params body closureEnv) args
  | length params /= length args =
      Left $ "*** ERROR: wrong number of arguments. Expected "
             ++ show (length params) ++ ", got " ++ show (length args)
  | otherwise =
      let extendedEnv = foldl
            (\env (p,a) -> defineVar p a env)
            closureEnv
            (zip params args)
      in eval extendedEnv body

apply _ notAFunction _ =
  Left $ "*** ERROR: tried to apply non-function: " ++ show notAFunction