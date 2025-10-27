module Main (main) where

import Linker.ObjectCompiler (irToObjectWithNames)
import Linker.Linker (linkObjectFiles)
import qualified Data.ByteString.Lazy as BSL
import Data.Binary (encode)
import Data.Bifunctor (first)
import AST.AST (Program(..), TopLevel(..), FunctionDecl(..), Function(..))

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
import Bytecode.Serialize (serializeProgramToStdout, loadProgramFromFile)
import qualified Error.Types
import Error.Types (formatCompilerError)

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
    , "    --compile        Compile to bytecode and write to stdout"
    , "    --compile-obj    Compile to object file and write to stdout"
    , "    --link FILES     Link object files and write bytecode to stdout"
    , "    --run FILE       Execute bytecode from FILE"
    , ""
    , "OPTIONS:"
    , "    --help           Display this help message"
    , "    --version        Display version information"
    , ""
    , "EXAMPLES:"
    , "    ./glados < program.c                   # Compile and execute"
    , "    ./glados --ast < program.c             # Show AST"
    , "    ./glados --ir < program.c              # Show human-readable IR"
    , "    ./glados --compile < program.c > a.gbc # Compile to bytecode"
    , "    ./glados --compile-obj < lib.c > lib.gbo     # Compile to object"
    , "    ./glados --link lib.gbo main.gbo > out.gbc   # Link objects"
    , "    ./glados --run a.gbc                   # Execute bytecode"
    , ""
    , "EXIT CODES:"
    , "    0    Success"
    , "    84   Error (parse, type, or runtime error)"
    ]

-- Print version information
printVersion :: IO ()
printVersion = putStrLn "GLaDOS v1.2"

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
    Left parseErr -> die (wrapParseError parseErr)
    Right ast -> case checkProgram ast of
        Left typeErrs -> die (wrapTypeErrors typeErrs)
        Right validatedAst -> case compileProgram validatedAst of
            Left compileErr -> die (formatCompilerError compileErr)
            Right irProgram -> case execute irProgram (mainIndex irProgram) [] of
                Left runtimeErr -> die (wrapRuntimeError runtimeErr)
                Right result -> print result
  where
    wrapParseError = formatCompilerError . Error.Types.wrapParseError
    wrapTypeErrors = formatCompilerError . Error.Types.wrapTypeErrors
    wrapRuntimeError = formatCompilerError . Error.Types.wrapRuntimeError

-- Output AST only
runAst :: String -> IO ()
runAst sourceCode = case parseProgram sourceCode of
    Left parseErr -> die (formatCompilerError (Error.Types.wrapParseError parseErr))
    Right ast -> print ast

-- Output human-readable IR (disassembly)
runIr :: String -> IO ()
runIr sourceCode = case parseProgram sourceCode of
    Left parseErr -> die (formatCompilerError (Error.Types.wrapParseError parseErr))
    Right ast -> case checkProgram ast of
        Left typeErrs -> die (formatCompilerError (Error.Types.wrapTypeErrors typeErrs))
        Right validatedAst -> case compileToText validatedAst of
            Left compileErr -> die (formatCompilerError compileErr)
            Right irText -> putStrLn irText

-- creates Bytecode
runCompile :: String -> IO ()
runCompile sourceCode = case parseProgram sourceCode of
    Left parseErr -> die (formatCompilerError (Error.Types.wrapParseError parseErr))
    Right ast -> case checkProgram ast of
        Left typeErrs -> die (formatCompilerError (Error.Types.wrapTypeErrors typeErrs))
        Right validatedAst -> case compileProgram validatedAst of
            Left compileErr -> die (formatCompilerError compileErr)
            Right irProgram -> serializeProgramToStdout irProgram

-- Loads existing Bytecode and executes
runBytecode :: FilePath -> IO ()
runBytecode path = do
    result <- loadProgramFromFile path
    case result of
        Left err -> die err
        Right irProgram -> case execute irProgram (mainIndex irProgram) [] of
            Left runtimeErr -> die $ formatCompilerError (Error.Types.wrapRuntimeError runtimeErr)
            Right value -> print value

-- Add these new functions
runCompileObject :: String -> IO ()
runCompileObject sourceCode = handleResult $ do
  ast <- first wrapParseError $ parseProgram sourceCode
  validatedAst <- first wrapTypeErrors $ checkProgram ast
  irProgram <- first formatCompilerError $ compileProgram validatedAst
  return $ buildObject irProgram validatedAst
  where
    wrapParseError = formatCompilerError . Error.Types.wrapParseError
    wrapTypeErrors = formatCompilerError . Error.Types.wrapTypeErrors
    buildObject irProgram validatedAst =
      irToObjectWithNames irProgram (extractAllFunctionNames validatedAst)
    handleResult (Left err) = die err
    handleResult (Right obj) = BSL.putStr (encode obj)

-- mm ordre que dans la func table ducp definitions first, then les protos sans definitions (externals)
extractAllFunctionNames :: Program -> [String]
extractAllFunctionNames (Program topLevels) = defs ++ externals
  where
    defs = [fName f | FuncDef f <- topLevels]
    protos = [fdName fd | FuncProto fd <- topLevels]
    externals = filter (`notElem` defs) protos

runLink :: [FilePath] -> IO ()
runLink objFiles = do
  result <- linkObjectFiles objFiles
  case result of
    Left err -> die $ "Link error: " ++ err
    Right linkedProg -> serializeProgramToStdout linkedProg

-- Main entry point
main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> readStdinWithTimeout >>= runDefault
        ["--help"] -> printHelp
        ["--version"] -> printVersion
        ["--ast"] -> readStdinWithTimeout >>= runAst
        ["--ir"] -> readStdinWithTimeout >>= runIr
        ["--compile"] -> readStdinWithTimeout >>= runCompile
        ["--compile-obj"] -> readStdinWithTimeout >>= runCompileObject
        ("--link":objFiles) | not (null objFiles) -> runLink objFiles
        ["--run", f] -> runBytecode f
        _ -> die "Invalid arguments. Use --help for usage information."

-- TODO: Create Hspec test cases for float operations (arithmetic, comparisons, type checking)
