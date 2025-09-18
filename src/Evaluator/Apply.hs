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

<<<<<<< HEAD
apply :: (Env -> LispVal -> Either String LispVal)
      -> LispVal -> [LispVal] -> Either String LispVal

-- Case 1: like (+, -, eq?, <, etc.)
apply _ (Builtin f) args =
  f args
=======
apply::(Env -> LispVal -> Either String (Env, LispVal))
    -> LispVal -> [LispVal] -> Either String LispVal

apply _ (Builtin f) args = f args
>>>>>>> dev_evaluator

apply eval (Function params body closureEnv) args
  | length params /= length args =
      Left $ "*** ERROR: wrong number of arguments. Expected "
             ++ show (length params) ++ ", got " ++ show (length args)
<<<<<<< HEAD
  | otherwise =
      let extendedEnv = foldl
            (\env (p,a) -> defineVar p a env)
            closureEnv
            (zip params args)
      in eval extendedEnv body

apply _ notAFunction _ =
  Left $ "*** ERROR: tried to apply non-function: " ++ show notAFunction
=======
    | otherwise =
        let newEnv = foldl (\env (p,a) -> defineVar p a env) closureEnv (zip params args)
        in do (_, result) <- eval newEnv body
              Right result

-- Non-function
apply _ notFn _ =
    Left $ "*** ERROR: tried to apply non-function: " ++ show notFn
>>>>>>> dev_evaluator
