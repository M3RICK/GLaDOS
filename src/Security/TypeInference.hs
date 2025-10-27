module Security.TypeInference (inferProgram) where

import qualified Data.Map as M
import AST.AST
import Security.Types
import Security.Environment
import Security.ExprChecker (getExprType)
import AST.Helpers (defaultPos, extractFunctions)

inferProgram :: Program -> Either [TypeError] Program
inferProgram (Program topLevels) = do
  let funcs = extractFunctions topLevels
  let fEnv = collectAllFunctionSignatures topLevels
  inferredFuncs <- mapM (inferFunction fEnv) funcs
  return (Program (reconstructTopLevels topLevels inferredFuncs))

reconstructTopLevels :: [TopLevel] -> [Function] -> [TopLevel]
reconstructTopLevels originals inferred =
  map (replaceFunc inferredMap) originals
  where
    inferredMap = M.fromList [(fName f, f) | f <- inferred]
    replaceFunc fMap (FuncDef f) = FuncDef (M.findWithDefault f (fName f) fMap)
    replaceFunc _ proto@(FuncProto _) = proto

-- single function
inferFunction :: FuncEnv -> Function -> Either [TypeError] Function
inferFunction fEnv func = do
  let env = makeFunctionEnv fEnv func
  inferredBody <- inferStatements env (fBody func)
  return $ func { fBody = inferredBody }

-- list of statements
inferStatements :: CheckEnv -> [Statement] -> Either [TypeError] [Statement]
inferStatements _ [] = Right []
inferStatements env (stmt:rest) = do
  (newEnv, inferredStmt) <- inferStatement env stmt
  inferredRest <- inferStatements newEnv rest
  return (inferredStmt : inferredRest)

inferStatement :: CheckEnv -> Statement -> Either [TypeError] (CheckEnv, Statement)
inferStatement env stmt = case stmt of
  Decl TypeInfer name (Just expr) -> inferTypeInferDecl env name expr
  Decl TypeInfer name Nothing -> handleUninitializedInfer name
  Decl typ name expr -> handleExplicitDecl env typ name expr
  Assign name expr -> handleAssignment env name expr
  If cond thenBody elseBody -> inferIfStatement env cond thenBody elseBody
  While cond body -> inferWhileStatement env cond body
  For initStmt cond updateStmt body -> inferForStatement env initStmt cond updateStmt body
  Return expr -> handleReturn env expr
  ExprStmt expr -> handleExprStmt env expr

inferTypeInferDecl :: CheckEnv -> String -> Expr -> Either [TypeError] (CheckEnv, Statement)
inferTypeInferDecl env name expr =
  case getExprType env expr of
    Left err -> Left [err]
    Right inferredType ->
      let newEnv = addVar name inferredType env
          finalEnv = markInitialized name newEnv
          newStmt = Decl inferredType name (Just expr)
      in Right (finalEnv, newStmt)

handleUninitializedInfer :: String -> Either [TypeError] (CheckEnv, Statement)
handleUninitializedInfer name = Left [createInferError name]

handleExplicitDecl :: CheckEnv -> Type -> String -> Maybe Expr -> Either [TypeError] (CheckEnv, Statement)
handleExplicitDecl env typ name expr =
  let newEnv = addVarAndMaybeInit name typ expr env
  in Right (newEnv, Decl typ name expr)

handleAssignment :: CheckEnv -> String -> Expr -> Either [TypeError] (CheckEnv, Statement)
handleAssignment env name expr = Right (env, Assign name expr)

handleReturn :: CheckEnv -> Expr -> Either [TypeError] (CheckEnv, Statement)
handleReturn env expr = Right (env, Return expr)

handleExprStmt :: CheckEnv -> Expr -> Either [TypeError] (CheckEnv, Statement)
handleExprStmt env expr = Right (env, ExprStmt expr)

inferForInit :: CheckEnv -> Maybe Statement -> Either [TypeError] (CheckEnv, Maybe Statement)
inferForInit env Nothing = Right (env, Nothing)
inferForInit env (Just stmt) = do
  (newEnv, inferredStmt) <- inferStatement env stmt
  Right (newEnv, Just inferredStmt)

inferForUpdate :: CheckEnv -> Maybe Statement -> Either [TypeError] (Maybe Statement)
inferForUpdate _ Nothing = Right Nothing
inferForUpdate env (Just stmt) = do
  (_, inferredStmt) <- inferStatement env stmt
  Right (Just inferredStmt)

-- add variable and mark as initialized if expr is present
addVarAndMaybeInit :: String -> Type -> Maybe Expr -> CheckEnv -> CheckEnv
addVarAndMaybeInit name typ maybeExpr env =
  let envWithVar = addVar name typ env
  in case maybeExpr of
    Nothing -> envWithVar
    Just _ -> markInitialized name envWithVar

-- Tu peux pas var sans init proprement
createInferError :: String -> TypeError
createInferError name =
  TypeMismatch TypeInfer TypeVoid defaultPos
    ("Cannot infer type for variable '" ++ name ++ "' without initialization")

inferIfStatement :: CheckEnv -> Expr -> [Statement] -> Maybe [Statement] -> Either [TypeError] (CheckEnv, Statement)
inferIfStatement env cond thenBody elseBody = do
  inferredThen <- inferStatements env thenBody
  inferredElse <- case elseBody of
    Nothing -> Right Nothing
    Just eb -> Just <$> inferStatements env eb
  Right (env, If cond inferredThen inferredElse)

inferWhileStatement :: CheckEnv -> Expr -> [Statement] -> Either [TypeError] (CheckEnv, Statement)
inferWhileStatement env cond body = do
  inferredBody <- inferStatements env body
  Right (env, While cond inferredBody)

inferForStatement :: CheckEnv -> Maybe Statement -> Maybe Expr -> Maybe Statement -> [Statement] -> Either [TypeError] (CheckEnv, Statement)
inferForStatement env initStmt cond updateStmt body = do
  (envAfterInit, inferredInit) <- inferForInit env initStmt
  inferredUpdate <- inferForUpdate envAfterInit updateStmt
  inferredBody <- inferStatements envAfterInit body
  Right (env, For inferredInit cond inferredUpdate inferredBody)
