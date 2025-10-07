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
usage = die "Usage:\n  ./glados <source>\n  ./glados --help"

runSource :: String -> IO ()
runSource s =
	case parseProgram s of
		Left e -> die $ "[Parse Error] " <> show e
		Right ast -> case ast of
			ASTInt n -> print n
			ASTBool b -> print b
			ASTExpression e -> case compileAndRun e of
				Right v -> print v
				Left er -> die $ "[Runtime/Compile Error] " <> show er
			_ -> die "[Error] Empty/unsupported program"

main :: IO ()
main = do
	a <- getArgs
	case a of
		["--help"] -> putStrLn "Usage:\n  ./glados <source>\n  ./glados --compile <source> -o <bytecode>\n  ./glados --run <bytecode>\n  ./glados --dump <bytecode>\n  ./glados --repl"
		[] -> getContents >>= runSource
		[p] -> readFile p >>= runSource
		_ -> usage