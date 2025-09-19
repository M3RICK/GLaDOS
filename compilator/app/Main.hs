module Main where

import System.Environment (getArgs)
import System.Exit
import System.IO (hPutStrLn, stderr)

import Compilator.Frontend.Parser   (parseProgram)
import Compilator.Compiler.Codegen  (compileModule)
import Compilator.Bytecode.Encode   (writeGlbc)
import Compilator.Bytecode.Disasm   (disasmModule)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["dump", src]               -> runDump src
    ["compile", src, "-o", out] -> runCompile src out
    _ -> putStrLn $ unlines
         [ "Usage:"
         , "  glc dump <source.my>"
         , "  glc compile <source.my> -o out.glbc"
         ]

runDump :: FilePath -> IO ()
runDump src = do
  s <- readFile src
  case parseProgram s of
    Left e  -> hPutStrLn stderr e >> exitWith (ExitFailure 84)
    Right p -> putStrLn (disasmModule (compileModule p))

runCompile :: FilePath -> FilePath -> IO ()
runCompile src out = do
  s <- readFile src
  case parseProgram s of
    Left e  -> hPutStrLn stderr e >> exitWith (ExitFailure 84)
    Right p -> writeGlbc out (compileModule p)
