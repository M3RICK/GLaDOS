-- Projet GLaDOS - Module Compiler  
-- Auteur : Copilot (Epitech style)
-- Description : Compilateur AST vers bytecode pour la Virtual Machine
-- Pipeline : Parser -> TypeChecker -> Compiler -> VirtualMachine

module Compiler (
    CompilerError(..), 
    compileExpression, compileStatement, compileProgram,
    compileAndRun, fullPipeline,
    testArithmetic, testBoolean
) where

import Parser (Expression(..), Statement(..), Function(..), Program(..), ArithOp(..), BoolOp(..))
import VirtualMachine (Instruction(..), Value(..), runVM, VMError(..))
import TypeChecker (typeCheckExpression, emptyEnv)

-- | Erreurs de compilation
data CompilerError = CompilerError String
                   | VMExecutionError VMError
    deriving (Show, Eq)

-- | Compilation d'une expression vers bytecode
compileExpression :: Expression -> [Instruction]
compileExpression expr = case expr of
    -- Literals
    EInt n -> [Push (VInt n)]
    EBool b -> [Push (VBool b)]
    
    -- Variables 
    EVar name -> [Load name]
    
    -- Opérations arithmétiques
    EArith left Add right -> 
        compileExpression left ++ compileExpression right ++ [AddI]
    EArith left Sub right -> 
        compileExpression left ++ compileExpression right ++ [SubI]
    EArith left Mul right -> 
        compileExpression left ++ compileExpression right ++ [MulI]
    EArith left Div right -> 
        compileExpression left ++ compileExpression right ++ [DivI]
    
    -- Opérations booléennes
    EBoolOp left Eq right -> 
        compileExpression left ++ compileExpression right ++ [EqI]
    EBoolOp left Ne right -> 
        compileExpression left ++ compileExpression right ++ [NeI]
    EBoolOp left Lt right -> 
        compileExpression left ++ compileExpression right ++ [LtI]
    EBoolOp left Gt right -> 
        compileExpression left ++ compileExpression right ++ [GtI]
    EBoolOp left Le right -> 
        compileExpression left ++ compileExpression right ++ [LeI]
    EBoolOp left Ge right -> 
        compileExpression left ++ compileExpression right ++ [GeI]
    EBoolOp left And right -> 
        compileExpression left ++ compileExpression right ++ [AndB]
    EBoolOp left Or right -> 
        compileExpression left ++ compileExpression right ++ [OrB]

-- | Compilation d'un statement vers bytecode
compileStatement :: Statement -> [Instruction]
compileStatement stmt = case stmt of
    -- Déclaration de variable (pas d'instruction VM nécessaire)
    SDecl _ _ -> []
    
    -- Assignment : expression puis store
    SAssign name expr -> 
        compileExpression expr ++ [Store name]
    
    -- Return statement
    SReturn expr -> 
        compileExpression expr ++ [Halt]
    
    -- If statement (version basique)
    SIf condition thenStmts elseStmts ->
        let thenCode = concatMap compileStatement thenStmts
            elseCode = concatMap compileStatement elseStmts
            thenLen = length thenCode
            elseLen = length elseCode
        in compileExpression condition ++
           [JumpIfNot (thenLen + 2)] ++  -- +2 pour Jump et l'instruction suivante
           thenCode ++
           [Jump elseLen] ++
           elseCode
    
    -- While loop (version basique)
    SWhile condition bodyStmts ->
        let bodyCode = concatMap compileStatement bodyStmts
            condCode = compileExpression condition
            bodyLen = length bodyCode
            condLen = length condCode
        in condCode ++
           [JumpIfNot (bodyLen + 2)] ++  -- Sortir si false
           bodyCode ++
           [Jump (-(bodyLen + condLen + 2))]  -- Retour au début

-- | Compilation d'une fonction (basique)
compileFunction :: Function -> [Instruction]
compileFunction (Function _ _ _ body) = 
    concatMap compileStatement body

-- | Compilation d'un programme complet
compileProgram :: Program -> [Instruction]
compileProgram (Program functions) = 
    concatMap compileFunction functions ++ [Halt]

-- | Compilation et exécution directe d'une expression
compileAndRun :: Expression -> Either CompilerError Value
compileAndRun expr = 
    case typeCheckExpression emptyEnv expr of
        Left typeErr -> Left (CompilerError $ "Type error: " ++ show typeErr)
        Right _ -> 
            let instructions = compileExpression expr ++ [Halt]
            in case runVM instructions of
                Left vmErr -> Left (VMExecutionError vmErr)
                Right result -> Right result

-- | Pipeline complet : parse -> typecheck -> compile -> execute
fullPipeline :: Expression -> Either CompilerError Value  
fullPipeline = compileAndRun

-- | Fonctions utilitaires pour les tests

-- | Test rapide d'une expression arithmétique
testArithmetic :: Int -> Int -> ArithOp -> [Instruction] 
testArithmetic a b op = 
    let expr = EArith (EInt a) op (EInt b)
    in compileExpression expr ++ [Halt]

-- | Test rapide d'une expression booléenne
testBoolean :: Int -> Int -> BoolOp -> [Instruction]
testBoolean a b op = 
    let expr = EBoolOp (EInt a) op (EInt b) 
    in compileExpression expr ++ [Halt]