module Security.Environment where

import qualified Data.Map as M
import qualified Data.Set as S
import Security.Types
import AST.AST

-- Create empty environment
emptyEnv :: CheckEnv
emptyEnv = CheckEnv M.empty M.empty Nothing S.empty

-- Add variable to environment
addVar :: String -> Type -> CheckEnv -> CheckEnv
addVar name typ env =
  env { varEnv = M.insert name typ (varEnv env) }

-- Lookup variable type
lookupVar :: String -> CheckEnv -> Maybe Type
lookupVar name env = M.lookup name (varEnv env)

-- Lookup function signature
lookupFunc :: String -> CheckEnv -> Maybe (Type, [Type])
lookupFunc name env = M.lookup name (funcEnv env)

-- Set current function return type
setReturnType :: Type -> CheckEnv -> CheckEnv
setReturnType typ env = env { currentReturnType = Just typ }

-- Build environment from function parameters
makeParamEnv :: [Parameter] -> VarEnv
makeParamEnv params =
  M.fromList [(paramName p, paramType p) | p <- params]

-- Create environment for function body
makeFunctionEnv :: FuncEnv -> Function -> CheckEnv
makeFunctionEnv fEnv func = CheckEnv
  { varEnv = makeParamEnv (fParams func)
  , funcEnv = fEnv
  , currentReturnType = Just (fType func)
  , initializedVars = S.fromList [paramName p | p <- fParams func]
  }

-- Collect all function signatures from program
collectFunctionSignatures :: [Function] -> FuncEnv
collectFunctionSignatures funcs =
  M.fromList [(fName f, getFunctionSignature f) | f <- funcs]

-- Get function signature as (return type, param types)
getFunctionSignature :: Function -> (Type, [Type])
getFunctionSignature func =
  (fType func, map paramType (fParams func))

-- Mark variable as initialized
markInitialized :: String -> CheckEnv -> CheckEnv
markInitialized name env =
  env { initializedVars = S.insert name (initializedVars env) }

-- Check if variable is initialized
isInitialized :: String -> CheckEnv -> Bool
isInitialized name env = S.member name (initializedVars env)
