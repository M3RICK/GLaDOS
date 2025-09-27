
-- Projet GLaDOS - Main executable  
-- Auteur : Copilot (Epitech style)
-- Description : Pipeline complet Parser -> TypeChecker -> Compiler -> VM

module Main (main) where

import Parser (Expression(..), ArithOp(..), BoolOp(..))
import TypeChecker (typeCheckExpression, emptyEnv)
import Compiler (compileAndRun, compileExpression)
import VirtualMachine (runVM, Instruction(..), Value(..))

-- | Pipeline complet : Parse -> TypeCheck -> Compile -> Execute
fullDemo :: String -> Expression -> IO ()
fullDemo desc expr = do
    putStrLn $ "\n--- " ++ desc ++ " ---"
    putStrLn $ "Expression: " ++ show expr
    
    -- Type checking
    case typeCheckExpression emptyEnv expr of
        Left typeErr -> putStrLn $ "Type error: " ++ show typeErr
        Right exprType -> do
            putStrLn $ "Type: " ++ show exprType
            
            -- Compilation
            let bytecode = compileExpression expr ++ [Halt]
            putStrLn $ "Bytecode: " ++ show bytecode
            
            -- Execution
            case runVM bytecode of
                Left vmErr -> putStrLn $ "VM error: " ++ show vmErr
                Right result -> putStrLn $ "Result: " ++ show result

-- | Demo simple avec compileAndRun
quickDemo :: String -> Expression -> IO ()
quickDemo desc expr = do
    putStrLn $ desc ++ ": " ++ show expr ++ " => " ++ 
        case compileAndRun expr of
            Left err -> "Error: " ++ show err
            Right result -> show result

main :: IO ()
main = do
    putStrLn "🚀 GLaDOS Compiler - Pipeline complet !"
    putStrLn "========================================"
    
    -- Démonstrations détaillées
    fullDemo "Addition simple" (EArith (EInt 10) Add (EInt 5))
    fullDemo "Expression complexe" (EArith (EArith (EInt 2) Mul (EInt 3)) Add (EInt 4))
    
    putStrLn "\n--- Tests rapides ---"
    -- Démonstrations rapides
    quickDemo "42" (EInt 42)
    quickDemo "true" (EBool True)
    quickDemo "3 * 7" (EArith (EInt 3) Mul (EInt 7))
    quickDemo "5 > 3" (EBoolOp (EInt 5) Gt (EInt 3))
    
    putStrLn "\n✅ Pipeline Parser -> TypeChecker -> Compiler -> VM fonctionnel !"