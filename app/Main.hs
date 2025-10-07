{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-TLS-5-1-glados-2 [WSL: Ubuntu-22.04]
-- File description:
-- Main
-}

module Main (main) where

import Parser (parseProgram, AST(..))
import Compiler (compileAndRun)

main :: IO ()
main = do
    input <- getLine
    case parseProgram input of
        Left _ -> return ()
        Right ast -> case ast of
            ASTInt n -> print n
            ASTBool b -> print b
            ASTExpression expr -> case compileAndRun expr of
                Right result -> print result
                Left _ -> return ()
            _ -> return ()