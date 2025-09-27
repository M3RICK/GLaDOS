-- Projet GLaDOS - Module TypeChecker
-- Auteur : Copilot (Epitech style)
-- Description : Vérification de type statique pour le langage GLaDOS
-- Fonctionnalités : type checking des expressions, statements, fonctions

module TypeChecker (
    TypeError(..), TypeEnv, 
    typeCheck, typeCheckExpression, typeCheckStatement, typeCheckFunction,
    emptyEnv, addVar, lookupVar
) where

import Parser (Type(..), Expression(..), Statement(..), Function(..), ArithOp(..), BoolOp(..))
import qualified Data.Map as Map

-- | Erreurs de type
data TypeError = 
    TypeMismatch Type Type String        -- attendu, reçu, contexte
  | UndefinedVariable String            -- nom de variable
  | RedefinedVariable String            -- nom de variable  
  | InvalidOperation String Type        -- opération, type
  | FunctionTypeMismatch String Type Type -- fonction, attendu, reçu
  deriving (Show, Eq)

-- | Environnement de types (table des symboles)
type TypeEnv = Map.Map String Type

-- | Environnement vide
emptyEnv :: TypeEnv
emptyEnv = Map.empty

-- | Ajouter une variable à l'environnement
addVar :: String -> Type -> TypeEnv -> Either TypeError TypeEnv
addVar name typ env = 
    case Map.lookup name env of
        Just _ -> Left (RedefinedVariable name)
        Nothing -> Right (Map.insert name typ env)

-- | Chercher le type d'une variable
lookupVar :: String -> TypeEnv -> Either TypeError Type
lookupVar name env = 
    case Map.lookup name env of
        Just typ -> Right typ
        Nothing -> Left (UndefinedVariable name)

-- | Type checking principal pour une expression
typeCheckExpression :: TypeEnv -> Expression -> Either TypeError Type
typeCheckExpression _env (EInt _) = Right TInt
typeCheckExpression _env (EBool _) = Right TBool
typeCheckExpression env (EVar name) = lookupVar name env

typeCheckExpression env (EArith left op right) = do
    leftType <- typeCheckExpression env left
    rightType <- typeCheckExpression env right
    case (leftType, rightType) of
        (TInt, TInt) -> Right TInt
        (TInt, other) -> Left (TypeMismatch TInt other ("arithmetic " ++ show op))
        (other, TInt) -> Left (TypeMismatch TInt other ("arithmetic " ++ show op))
        (other1, other2) -> Left (TypeMismatch TInt other1 ("arithmetic " ++ show op))

typeCheckExpression env (EBoolOp left op right) = do
    leftType <- typeCheckExpression env left
    rightType <- typeCheckExpression env right
    case op of
        -- Opérateurs de comparaison : int -> int -> bool
        Lt -> case (leftType, rightType) of
            (TInt, TInt) -> Right TBool
            (TInt, other) -> Left (TypeMismatch TInt other "comparison")
            (other, _) -> Left (TypeMismatch TInt other "comparison")
        Gt -> case (leftType, rightType) of
            (TInt, TInt) -> Right TBool
            (TInt, other) -> Left (TypeMismatch TInt other "comparison")
            (other, _) -> Left (TypeMismatch TInt other "comparison")
        Le -> case (leftType, rightType) of
            (TInt, TInt) -> Right TBool
            (TInt, other) -> Left (TypeMismatch TInt other "comparison")
            (other, _) -> Left (TypeMismatch TInt other "comparison")
        Ge -> case (leftType, rightType) of
            (TInt, TInt) -> Right TBool
            (TInt, other) -> Left (TypeMismatch TInt other "comparison")
            (other, _) -> Left (TypeMismatch TInt other "comparison")
        -- Opérateurs d'égalité : même type -> bool
        Eq -> if leftType == rightType 
              then Right TBool 
              else Left (TypeMismatch leftType rightType "equality")
        Ne -> if leftType == rightType 
              then Right TBool 
              else Left (TypeMismatch leftType rightType "equality")
        -- Opérateurs logiques : bool -> bool -> bool
        And -> case (leftType, rightType) of
            (TBool, TBool) -> Right TBool
            (TBool, other) -> Left (TypeMismatch TBool other "logical AND")
            (other, _) -> Left (TypeMismatch TBool other "logical AND")
        Or -> case (leftType, rightType) of
            (TBool, TBool) -> Right TBool
            (TBool, other) -> Left (TypeMismatch TBool other "logical OR")
            (other, _) -> Left (TypeMismatch TBool other "logical OR")

-- | Type checking pour un statement
typeCheckStatement :: TypeEnv -> Statement -> Either TypeError TypeEnv
-- Déclaration de variable
typeCheckStatement env (SDecl typ name) = addVar name typ env

-- Assignment de variable
typeCheckStatement env (SAssign name expr) = do
    varType <- lookupVar name env
    exprType <- typeCheckExpression env expr
    if varType == exprType
        then Right env
        else Left (TypeMismatch varType exprType ("assignment to " ++ name))

-- Return statement
typeCheckStatement env (SReturn expr) = do
    _exprType <- typeCheckExpression env expr
    Right env  -- TODO: vérifier avec le type de retour de la fonction

-- If statement
typeCheckStatement env (SIf condition thenStmts elseStmts) = do
    condType <- typeCheckExpression env condition
    case condType of
        TBool -> do
            _thenEnv <- typeCheckStatements env thenStmts
            _elseEnv <- typeCheckStatements env elseStmts
            Right env  -- TODO: merger les environnements
        other -> Left (TypeMismatch TBool other "if condition")

-- While statement
typeCheckStatement env (SWhile condition bodyStmts) = do
    condType <- typeCheckExpression env condition
    case condType of
        TBool -> do
            _bodyEnv <- typeCheckStatements env bodyStmts
            Right env
        other -> Left (TypeMismatch TBool other "while condition")

-- | Type checking pour une liste de statements
typeCheckStatements :: TypeEnv -> [Statement] -> Either TypeError TypeEnv
typeCheckStatements env [] = Right env
typeCheckStatements env (stmt:stmts) = do
    newEnv <- typeCheckStatement env stmt
    typeCheckStatements newEnv stmts

-- | Type checking pour une fonction
typeCheckFunction :: TypeEnv -> Function -> Either TypeError TypeEnv
typeCheckFunction env (Function retType name params body) = do
    -- Ajouter les paramètres à l'environnement
    paramEnv <- foldl addParam (Right env) params
    -- Vérifier le corps de la fonction
    _bodyEnv <- typeCheckStatements paramEnv body
    -- TODO: vérifier que tous les chemins retournent le bon type
    Right env
  where
    addParam (Left err) _ = Left err
    addParam (Right currentEnv) (paramType, paramName) = addVar paramName paramType currentEnv

-- | Type checking principal (placeholder)
typeCheck :: Expression -> Either TypeError Type
typeCheck expr = typeCheckExpression emptyEnv expr