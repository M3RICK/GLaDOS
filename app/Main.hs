module Main (main) where

import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import Parser.Core (parseProgram)
import Security.TypeChecker (checkProgram)
import Compiler.Core (compileProgram)
import VM.Interpreter (execute)
import IR.Types (mainIndex)
import Security.ErrorFormat (formatError)

main :: IO ()
main = do
    sourceCode <- getContents
    case parseProgram sourceCode of
        Left parseErr -> do
            hPutStrLn stderr $ "Parse error: " ++ show parseErr
            exitWith (ExitFailure 84)
        Right ast ->
            case checkProgram ast of
                Left typeErrs -> do
                    hPutStrLn stderr "Type errors:"
                    mapM_ (hPutStrLn stderr . formatError) typeErrs
                    exitWith (ExitFailure 84)
                Right validatedAst -> do
                    let irProgram = compileProgram validatedAst
                    case execute irProgram (mainIndex irProgram) [] of
                        Left runtimeErr -> do
                            hPutStrLn stderr $ "Runtime error: " ++ runtimeErr
                            exitWith (ExitFailure 84)
                        Right result -> do
                            print result
                            exitWith ExitSuccess
