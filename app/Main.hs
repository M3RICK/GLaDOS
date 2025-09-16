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
--         0 -> putStrLn "damn0"
--         2 -> putStrLn "what2"
--         3 -> putStrLn "hunh3"

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> putStrLn "damn0"
        1 -> putStrLn "wow1"
        2 -> putStrLn "what2"
        3 -> putStrLn "hunh3"
        _ -> putStrLn "unhandled number of arguments"
