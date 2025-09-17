module Main (main) where

import System.IO
import System.Exit
import System.Environment (getArgs)
import Types
import Environment  
import Builtins
import qualified Parser.Core as Core
import qualified Parser.Atomic as Atomic
import qualified Parser.Compound as Compound
import qualified Parser.Helpers as Helpers
-- import evaluator

-- main :: IO ()
-- main = do
--     args <- getArgs
--     input <- case length args of
--         0 -> getContents
--         1 -> readFile (head args)
--         _ -> putStrLn "Usage error" >> exitWith (ExitFailure 84) >> return "" --need to return this empty string because expecting a IO string cant simply just exit
--     Core.readExpr


main :: IO ()
main = do
  args <- getArgs
  input <- case length args of
    0 -> getContents
    1 -> readFile (head args)
    _ -> putStrLn "Usage error" >> exitWith (ExitFailure 84) >> return ""
  case Core.readProgram input of
    Left parseError -> putStrLn (show parseError) >> exitWith (ExitFailure 84)
    Right expressions -> mapM_ (putStrLn . show) expressions  -- Print each parsed expression