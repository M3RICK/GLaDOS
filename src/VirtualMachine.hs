{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-TLS-5-1-glados-2 [WSL: Ubuntu-22.04]
-- File description:
-- VirtualMachine
-}

-- Projet GLaDOS - Module Virtual Machine
-- Description : Machine virtuelle stack-based pour exécuter le bytecode
-- Architecture : Stack VM avec pile d'exécution et mémoire locale

module VirtualMachine (
    -- Types principaux
    Instruction(..), Value(..), VMState(..), VMError(..),
    -- Fonctions principales
    runVM, executeInstruction, executeProgram,
    -- Fonctions utilitaires
    emptyVMState, pushStack, popStack, peekStack,
    getVar, setVar, clearStack
) where

import qualified Data.Map as Map
import Data.Map (Map)

-- | Valeurs supportées par la VM
data Value = VInt Int
           | VBool Bool
           deriving (Show, Eq)

-- | Instructions de bytecode pour la VM
data Instruction = -- Manipulation de pile
                  Push Value              -- Empiler une valeur
                | Pop                     -- Dépiler une valeur
                | Dup                     -- Dupliquer le sommet de pile
                -- Opérations arithmétiques
                | AddI                    -- Addition d'entiers
                | SubI                    -- Soustraction d'entiers  
                | MulI                    -- Multiplication d'entiers
                | DivI                    -- Division d'entiers
                -- Opérations booléennes
                | EqI                     -- Égalité
                | NeI                     -- Inégalité
                | LtI                     -- Inférieur
                | GtI                     -- Supérieur
                | LeI                     -- Inférieur ou égal
                | GeI                     -- Supérieur ou égal
                | AndB                    -- ET logique
                | OrB                     -- OU logique
                | NotB                    -- NON logique
                -- Manipulation de variables
                | Load String             -- Charger une variable sur la pile
                | Store String            -- Stocker le sommet de pile dans une variable
                -- Contrôle de flot
                | Jump Int                -- Saut inconditionnel
                | JumpIf Int              -- Saut conditionnel si vrai
                | JumpIfNot Int           -- Saut conditionnel si faux
                | Call String             -- Appel de fonction
                | Return                  -- Retour de fonction
                | Halt                    -- Arrêt de la VM
    deriving (Show, Eq)

-- | Erreurs possibles de la VM
data VMError = StackUnderflow             -- Pile vide lors d'un pop
             | DivisionByZero             -- Division par zéro
             | UndefinedVariable String   -- Variable non définie
             | TypeMismatch String        -- Erreur de type
             | InvalidJump Int            -- Saut vers une adresse invalide
             | UnknownInstruction         -- Instruction inconnue
             | RuntimeError String        -- Erreur générale d'exécution
    deriving (Show, Eq)

-- | État de la machine virtuelle
data VMState = VMState {
    stack :: [Value],                     -- Pile d'exécution
    variables :: Map String Value,        -- Variables locales
    programCounter :: Int,                -- Compteur de programme
    program :: [Instruction],             -- Programme à exécuter
    callStack :: [Int],                   -- Pile d'appels pour les fonctions
    running :: Bool                       -- État de la VM (en cours/arrêtée)
} deriving (Show, Eq)

-- | État initial de la VM
emptyVMState :: [Instruction] -> VMState
emptyVMState instructions = VMState {
    stack = [],
    variables = Map.empty,
    programCounter = 0,
    program = instructions,
    callStack = [],
    running = True
}

-- | Exécution principale de la VM
runVM :: [Instruction] -> Either VMError Value
runVM instructions = executeProgram (emptyVMState instructions)

-- | Exécution d'un programme complet
executeProgram :: VMState -> Either VMError Value
executeProgram state
    | not (running state) = case stack state of
        (result:_) -> Right result
        [] -> Left (RuntimeError "Program ended with empty stack")
    | programCounter state >= length (program state) = 
        Left (RuntimeError "Program counter out of bounds")
    | otherwise = do
        let currentInstruction = program state !! programCounter state
        newState <- executeInstruction currentInstruction state
        executeProgram newState

-- | Exécution d'une instruction
executeInstruction :: Instruction -> VMState -> Either VMError VMState
executeInstruction instruction state = case instruction of
    -- Manipulation de pile
    Push val -> Right $ state { 
        stack = val : stack state,
        programCounter = programCounter state + 1 
    }
    
    Pop -> case stack state of
        (_:rest) -> Right $ state { 
            stack = rest,
            programCounter = programCounter state + 1 
        }
        [] -> Left StackUnderflow
    
    Dup -> case stack state of
        (val:_) -> Right $ state { 
            stack = val : stack state,
            programCounter = programCounter state + 1 
        }
        [] -> Left StackUnderflow
    
    -- Opérations arithmétiques
    AddI -> binaryArithOp (+) state
    SubI -> binaryArithOp (-) state  
    MulI -> binaryArithOp (*) state
    DivI -> case stack state of
        (VInt 0:VInt _:_) -> Left DivisionByZero
        _ -> binaryArithOp div state
    
    -- Opérations de comparaison
    EqI -> binaryCompOp (==) state
    NeI -> binaryCompOp (/=) state
    LtI -> binaryCompOp (<) state
    GtI -> binaryCompOp (>) state
    LeI -> binaryCompOp (<=) state
    GeI -> binaryCompOp (>=) state
    
    -- Opérations booléennes
    AndB -> binaryBoolOp (&&) state
    OrB -> binaryBoolOp (||) state
    NotB -> case stack state of
        (VBool b:rest) -> Right $ state {
            stack = VBool (not b) : rest,
            programCounter = programCounter state + 1
        }
        (VInt _:_) -> Left (TypeMismatch "Expected boolean for NOT operation")
        [] -> Left StackUnderflow
    
    -- Manipulation de variables
    Load varName -> case Map.lookup varName (variables state) of
        Just val -> Right $ state {
            stack = val : stack state,
            programCounter = programCounter state + 1
        }
        Nothing -> Left (UndefinedVariable varName)
    
    Store varName -> case stack state of
        (val:rest) -> Right $ state {
            stack = rest,
            variables = Map.insert varName val (variables state),
            programCounter = programCounter state + 1
        }
        [] -> Left StackUnderflow
    
    -- Contrôle de flot
    Jump addr -> 
        if addr >= 0 && addr < length (program state)
        then Right $ state { programCounter = addr }
        else Left (InvalidJump addr)
    
    JumpIf addr -> case stack state of
        (VBool True:rest) -> 
            if addr >= 0 && addr < length (program state)
            then Right $ state { stack = rest, programCounter = addr }
            else Left (InvalidJump addr)
        (VBool False:rest) -> Right $ state { 
            stack = rest, 
            programCounter = programCounter state + 1 
        }
        (VInt _:_) -> Left (TypeMismatch "Expected boolean for conditional jump")
        [] -> Left StackUnderflow
    
    JumpIfNot addr -> case stack state of
        (VBool False:rest) -> 
            if addr >= 0 && addr < length (program state)
            then Right $ state { stack = rest, programCounter = addr }
            else Left (InvalidJump addr)
        (VBool True:rest) -> Right $ state { 
            stack = rest, 
            programCounter = programCounter state + 1 
        }
        (VInt _:_) -> Left (TypeMismatch "Expected boolean for conditional jump")
        [] -> Left StackUnderflow
    
    -- Fonctions (basique pour l'instant)
    Call _ -> Left (RuntimeError "Function calls not yet implemented")
    Return -> Left (RuntimeError "Function returns not yet implemented")
    
    -- Arrêt
    Halt -> Right $ state { running = False }

-- | Opération arithmétique binaire
binaryArithOp :: (Int -> Int -> Int) -> VMState -> Either VMError VMState
binaryArithOp op state = case stack state of
    (VInt b:VInt a:rest) -> Right $ state {
        stack = VInt (a `op` b) : rest,
        programCounter = programCounter state + 1
    }
    (VBool _:_:_) -> Left (TypeMismatch "Expected integers for arithmetic operation")
    (_:VBool _:_) -> Left (TypeMismatch "Expected integers for arithmetic operation")
    (_:[]) -> Left StackUnderflow
    [] -> Left StackUnderflow

-- | Opération de comparaison binaire
binaryCompOp :: (Int -> Int -> Bool) -> VMState -> Either VMError VMState
binaryCompOp op state = case stack state of
    (VInt b:VInt a:rest) -> Right $ state {
        stack = VBool (a `op` b) : rest,
        programCounter = programCounter state + 1
    }
    (VBool _:_:_) -> Left (TypeMismatch "Expected integers for comparison")
    (_:VBool _:_) -> Left (TypeMismatch "Expected integers for comparison")  
    (_:[]) -> Left StackUnderflow
    [] -> Left StackUnderflow

-- | Opération booléenne binaire
binaryBoolOp :: (Bool -> Bool -> Bool) -> VMState -> Either VMError VMState
binaryBoolOp op state = case stack state of
    (VBool b:VBool a:rest) -> Right $ state {
        stack = VBool (a `op` b) : rest,
        programCounter = programCounter state + 1
    }
    (VInt _:_:_) -> Left (TypeMismatch "Expected booleans for logical operation")
    (_:VInt _:_) -> Left (TypeMismatch "Expected booleans for logical operation")
    (_:[]) -> Left StackUnderflow
    [] -> Left StackUnderflow

-- | Fonctions utilitaires pour la manipulation de pile
pushStack :: Value -> VMState -> VMState
pushStack val state = state { stack = val : stack state }

popStack :: VMState -> Either VMError (Value, VMState)
popStack state = case stack state of
    (val:rest) -> Right (val, state { stack = rest })
    [] -> Left StackUnderflow

peekStack :: VMState -> Either VMError Value
peekStack state = case stack state of
    (val:_) -> Right val
    [] -> Left StackUnderflow

-- | Manipulation des variables
getVar :: String -> VMState -> Either VMError Value
getVar varName state = case Map.lookup varName (variables state) of
    Just val -> Right val
    Nothing -> Left (UndefinedVariable varName)

setVar :: String -> Value -> VMState -> VMState
setVar varName val state = state { 
    variables = Map.insert varName val (variables state) 
}

-- | Nettoyage de la pile
clearStack :: VMState -> VMState
clearStack state = state { stack = [] }