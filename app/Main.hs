{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- Main
-}

module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import Parser (parseProgram, AST(..))
import Compiler (compileAndRun, compileExpression)

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitWith (ExitFailure 84)

usage :: IO a
usage = die "Usage:\n  ./glados <source>\n  ./glados --compile <source> -o <bytecode>\n  ./glados --run <bytecode>\n  ./glados --help"

runSource :: String -> IO ()
runSource s = case parseProgram s of
    Left e -> die $ "[Parse Error] " <> show e
    Right ast -> case ast of
        ASTInt n -> print n
        ASTBool b -> print b
        ASTExpression e -> case compileAndRun e of
            Right v -> print v
            Left er -> die $ "[Runtime Error] " <> show er
        _ -> die "[Error] Empty program"

-- compile vers file bytecod
compileFile :: FilePath -> FilePath -> IO ()
compileFile src out = do
    s <- readFile src
    case parseProgram s of
        Left e -> die $ "[Parse Error] " <> show e
        Right ast -> case ast of
            ASTExpression expr -> do
                let bytecode = compileExpression expr
                writeFile out (show bytecode)
                putStrLn $ "Compiled " <> src <> " to " <> out
            _ -> die "[Error] Can only compile expressions for now"

runBytecode :: FilePath -> IO ()
runBytecode p = do
    content <- readFile p
    case parseProgram content of
        Left e -> die $ "[Parse Error] " <> show e
        Right ast -> case ast of
            ASTExpression expr -> case compileAndRun expr of
                Right val -> print val
                Left err -> die $ "[VM Error] " <> show err
            ASTInt n -> print n
            ASTBool b -> print b
            _ -> die "[Error] Can't run this"

main :: IO ()
main = do
    a <- getArgs
    case a of
        ["--help"] -> putStrLn "GLaDOS compiler\n\nUsage:\n  ./glados <source>           - run source\n  ./glados --compile <src> -o <out> - compile to bytecode\n  ./glados --run <bytecode>   - run bytecode"
        [] -> getContents >>= runSource
        [p] -> readFile p >>= runSource
        ["--compile", s, "-o", o] -> compileFile s o
        ["--run", b] -> runBytecode b
        _ -> usage
