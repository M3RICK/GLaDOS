module Evaluator.Apply (apply) where

import Types
import Environment

type Evaluator = Env -> LispVal -> Either String (Env, LispVal)

apply :: Evaluator -> LispVal -> [LispVal] -> Either String LispVal
apply _ (Builtin f) args = f args
apply eval (Function params body closureEnv) args
  | wrongArity params args = Left (arityMessage params args)
  | otherwise =
      let newEnv = extendEnv closureEnv params args
      in evalBody eval newEnv body
apply _ notFn _ = Left (notFunctionMessage notFn)


-- Helpers pour apply

wrongArity :: [String] -> [LispVal] -> Bool
wrongArity params args = length params /= length args

arityMessage :: [String] -> [LispVal] -> String
arityMessage params args =
  "*** ERROR: wrong number of arguments. Expected "
  ++ show (length params) ++ ", got " ++ show (length args)

extendEnv :: Env -> [String] -> [LispVal] -> Env
extendEnv env [] [] = env
extendEnv env (p:ps) (a:as) =
  let newEnv = defineVar p a env
  in extendEnv newEnv ps as
extendEnv env _ _ = env

evalBody :: Evaluator -> Env -> LispVal -> Either String LispVal
evalBody eval env body =
  case eval env body of
    Left err        -> Left err
    Right (_, val)  -> Right val

notFunctionMessage :: LispVal -> String
notFunctionMessage notFn =
  "*** ERROR: tried to apply non-function: " ++ show notFn
