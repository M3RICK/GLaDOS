{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-TLS-5-1-glados-2 [WSL: Ubuntu-22.04]
-- File description:
-- Main
-}

module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import Parser (parseProgram, AST(..))
import Compiler (compileAndRun)

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitWith (ExitFailure 84)

usage :: IO a
usage = die "Usage:\n  ./glados <source>\n  ./glados --compile <source> -o <bytecode>\n  ./glados --run <bytecode>\n  ./glados --help"

runSource :: String -> IO ()
runSource s =
	case parseProgram s of
		Left e -> die $ "[Parse Error] " <> show e
		Right ast -> case ast of
			ASTInt n -> print n
			ASTBool b -> print b
			ASTExpression e -> case compileAndRun e of
				Right v -> print v
				Left er -> die $ "[Runtime Error] " <> show er
			_ -> die "[Error] Empty program"

-- fake pour l'instant a ne pas regardr mdr
compileFile :: FilePath -> FilePath -> IO ()
compileFile src out = do
	s <- readFile src
	case parseProgram s of
		Left e -> die $ "[Parse Error] " <> show e
		Right ast -> do
			writeFile out (show ast)
			putStrLn $ "Compiled to " <> out

runBytecode :: FilePath -> IO ()
runBytecode p = do
	content <- readFile p
	putStrLn $ "Running bytecode from " <> p
	putStrLn "42"  -- fake aussi pour l'outpout

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