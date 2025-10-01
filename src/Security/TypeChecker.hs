module Security.TypeChecker where

import Security.Types
import Security.Environment
import Security.StatementCheckers
import AST.AST

checkProgram :: Program -> Either [TypeError] Program
checkProgram prog@(Program funcs) =
  let errors = checkAllFunctions funcs
  in if null errors then Right prog else Left errors

checkAllFunctions :: [Function] -> [TypeError]
checkAllFunctions funcs =
  let funcEnv = collectFunctionSignatures funcs
  in concatMap (checkFunction funcEnv) funcs

checkFunction :: FuncEnv -> Function -> [TypeError]
checkFunction funcEnv func =
  let env = makeFunctionEnv funcEnv func
  in checkStatements env (fBody func)
