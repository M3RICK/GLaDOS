module Security.TypeChecker where

import Security.Types
import Security.Environment
import Security.StatementCheckers
import AST.AST
import Security.ReturnChecker (listHasReturn)

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
      stmtErrors = checkStatements env (fBody func)
      returnError = checkFunctionReturns func
  in stmtErrors ++ returnError

checkFunctionReturns :: Function -> [TypeError]
checkFunctionReturns func
  | fType func == TypeVoid = []  -- Si void function pas besoin de return
  | listHasReturn (fBody func) = [] -- c est bueno
  | otherwise = [MissingReturn (fName func)] -- c est pas bueno
