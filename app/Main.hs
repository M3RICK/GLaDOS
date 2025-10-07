{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-TLS-5-1-glados-2 [WSL: Ubuntu-22.04]
-- File description:
-- Main
-}

module Main (main) where

import System.IO (hFlush, stdout)
import Parser (parseProgram, AST(..))
import Compiler (compileAndRun)

-- | Fonction principale simple
main :: IO ()
main = do
    putStr "GLaDOS> "
    hFlush stdout
    input <- getLine
    
    if null input
        then putStrLn "Empty input"
        else processInput input

-- | Traite l'entrée utilisateur
processInput :: String -> IO ()
processInput input = do
    case parseProgram input of
        Left parseErr -> putStrLn $ "Parse error: " ++ parseErr
        Right ast -> executeAST ast

-- | Exécute l'AST
executeAST :: AST -> IO ()
executeAST (ASTInt n) = print n
executeAST (ASTBool b) = print b
executeAST (ASTExpression expr) = do
    case compileAndRun expr of
        Left err -> putStrLn $ "Error: " ++ show err
        Right result -> print result
executeAST (ASTProgram _) = putStrLn "Program execution not implemented yet"