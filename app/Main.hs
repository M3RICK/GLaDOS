-- module Main (main) where

-- import System.IO
-- import System.Exit
-- import System.Environment (getArgs)
-- import Types
-- import Environment  
-- import Builtins
-- import qualified Parser.Core as Core
-- import qualified Parser.Atomic as Atomic
-- import qualified Parser.Compound as Compound
-- import qualified Parser.Helpers as Helpers
-- -- import evaluator

-- -- main :: IO ()
-- -- main = do
-- --     args <- getArgs
-- --     input <- case length args of
-- --         0 -> getContents
-- --         1 -> readFile (head args)
-- --         _ -> putStrLn "Usage error" >> exitWith (ExitFailure 84) >> return "" --need to return this empty string because expecting a IO string cant simply just exit
-- --     Core.readExpr


-- main :: IO ()
-- main = do
--   args <- getArgs
--   input <- case length args of
--     0 -> getContents
--     1 -> readFile (head args)
--     _ -> putStrLn "Usage error" >> exitWith (ExitFailure 84) >> return ""
--   case Core.readProgram input of
--     Left parseError -> putStrLn (show parseError) >> exitWith (ExitFailure 84)
--     Right expressions -> mapM_ (putStrLn . show) expressions  -- Print each parsed expression




module Main where

import System.Environment (getArgs)
import System.IO
import System.Exit
import Types
import Environment
import Builtins
import qualified Parser.Core as Core
import qualified Evaluator.Core as Evaluator
import Text.Parsec (ParseError)

main :: IO ()
main = do
  args <- getArgs
  input <- case length args of
    0 -> getContents
    1 -> readFile (head args)
    _ -> putStrLn "Usage error" >> exitWith (ExitFailure 84) >> return ""
  
  case Core.readProgram input of
    Left parseError -> do
      putStrLn $ "Parse error: " ++ show parseError
      exitWith (ExitFailure 84)
    Right expressions -> do
      result <- processExpressions initialEnv expressions
      case result of
        Left err -> putStrLn err >> exitWith (ExitFailure 84)
        Right (finalEnv, finalValue) -> print finalValue

-- Process expressions sequentially, maintaining environment state
processExpressions :: Env -> [LispVal] -> IO (Either String (Env, LispVal))
processExpressions env [] = return $ Right (env, Bool True)
processExpressions env [expr] = do
  case Evaluator.eval env expr of
    Left err -> return $ Left err
    Right val -> return $ Right (env, val)
processExpressions env (expr:rest) = do
  case Evaluator.eval env expr of
    Left err -> return $ Left err
    Right val -> do
      -- For define expressions, we need to extract the updated environment
      -- This is a workaround for the current evaluator structure
      let updatedEnv = case expr of
            List (Atom "define" : Atom var : _) ->
              defineVar var val env  -- Use the 'val' we already computed
            _ -> env
      processExpressions updatedEnv rest