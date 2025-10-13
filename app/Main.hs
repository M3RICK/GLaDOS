module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Parser.Core (parseProgram)
import Security.TypeChecker (checkProgram)
import Compiler.Core (compileProgram, compileToText)
import VM.Interpreter (execute)
import IR.Types (mainIndex)

-- Exit with error code 84
die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitWith (ExitFailure 84)

-- Print help message and exit successfully
printHelp :: IO ()
printHelp = putStr $ unlines
    [ "GLaDOS - Generic Language and Data Operand Syntax"
    , ""
    , "USAGE:"
    , "    ./glados [OPTIONS]"
    , ""
    , "MODES:"
    , "    (default)        Read from stdin, compile and execute"
    , "    --ast            Parse and display the Abstract Syntax Tree"
    , "    --ir             Parse, compile and display human-readable IR"
    , "    --compile        Compile to bytecode and write to stdout (TODO)"
    , "    --run FILE       Execute bytecode from FILE (TODO)"
    , ""
    , "OPTIONS:"
    , "    --help           Display this help message"
    , ""
    , "EXAMPLES:"
    , "    ./glados < program.c              # Compile and execute"
    , "    ./glados --ast < program.c        # Show AST"
    , "    ./glados --ir < program.c         # Show human-readable IR"
    , "    ./glados --compile < program.c    # Output bytecode (TODO)"
    , "    ./glados --run bytecode.bc        # Run bytecode (TODO)"
    , ""
    , "EXIT CODES:"
    , "    0    Success"
    , "    84   Error (parse, type, or runtime error)"
    ]

-- Read stdin with 1 second timeout (force strict evaluation)
readStdinWithTimeout :: IO String
readStdinWithTimeout = timeout 1000000 (getContents >>= evaluate . force) >>= \result -> case result of
    Nothing -> die "Error: No input provided"
    Just content -> case content of
        "" -> die "Error: No input provided"
        _ -> return content

-- Default compile and execute
runDefault :: String -> IO ()
runDefault sourceCode = case parseProgram sourceCode of
    Left _ -> die "Parse error"
    Right ast -> case checkProgram ast of
        Left _ -> die "Type error"
        Right validatedAst -> let irProgram = compileProgram validatedAst
            in case execute irProgram (mainIndex irProgram) [] of
                Left _ -> die "Runtime error"
                Right result -> print result

-- Output AST only
runAst :: String -> IO ()
runAst sourceCode = case parseProgram sourceCode of
    Left _ -> die "Parse error"
    Right ast -> print ast

-- Output human-readable IR (disassembly)
runIr :: String -> IO ()
runIr sourceCode = case parseProgram sourceCode of
    Left _ -> die "Parse error"
    Right ast -> case checkProgram ast of
        Left _ -> die "Type error"
        Right validatedAst -> putStrLn (compileToText validatedAst)

-- Compile to bytecode (TODO - needs Binary serialization)
runCompile :: String -> IO ()
runCompile _sourceCode =
    die "Error: Bytecode compilation not yet implemented\nTODO: Implement Binary serialization for IRProgram"

-- Execute bytecode file (TODO - needs Binary deserialization)
runBytecode :: FilePath -> IO ()
runBytecode _path =
    die "Error: Bytecode execution not yet implemented\nTODO: Implement Binary deserialization for IRProgram"

-- Main entry point
main :: IO ()
main = do
    args <- getArgs
    case args of
        []            -> readStdinWithTimeout >>= runDefault
        ["--help"]    -> printHelp
        ["--ast"]     -> readStdinWithTimeout >>= runAst
        ["--ir"]      -> readStdinWithTimeout >>= runIr
        ["--compile"] -> readStdinWithTimeout >>= runCompile
        ["--run", f]  -> runBytecode f
        _             -> die "Invalid arguments. Use --help for usage information."


-- TODO: AYMERIC DONT FORGET TO ADD YOUR CUSTOM SECURITY ERRORS IN HERE