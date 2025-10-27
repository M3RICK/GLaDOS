module Security.TypeChecker (checkProgram) where

import Security.Types
import Security.Environment
import Security.StatementCheckers
import Security.TypeInference (inferProgram)
import AST.AST
import AST.Helpers (extractFunctions)
import Security.ReturnChecker (listHasReturn)

checkProgram :: Program -> Either [TypeError] Program
checkProgram prog =
  case inferProgram prog of
    Left err -> Left err
    Right inferredProg ->
      if null errors
        then Right inferredProg
        else Left errors
      where
        Program topLevels = inferredProg
        funcs = extractFunctions topLevels
        fEnv = collectAllFunctionSignatures topLevels
        errors = concatMap (checkFunction fEnv) funcs

checkFunction :: FuncEnv -> Function -> [TypeError]
checkFunction fEnv func =
  let env = makeFunctionEnv fEnv func
      stmtErrors = checkStatements env (fBody func)
      returnError = checkFunctionReturns func
  in stmtErrors ++ returnError

checkFunctionReturns :: Function -> [TypeError]
checkFunctionReturns func
  | fType func == TypeVoid = []  -- Si void function pas besoin de return
  | listHasReturn (fBody func) = [] -- c est bueno
  | otherwise = [MissingReturn (fName func)] -- c est pas bueno
