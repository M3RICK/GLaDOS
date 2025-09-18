module Main (main) where
import System.Environment (getArgs)
import System.Exit
import Types
import Environment
import Builtins
import qualified Parser.Core as Core
import qualified Evaluator.Core as Evaluator

main :: IO ()
main = do
  args <- getArgs
  input <- case length args of
    0 -> getContents
    1 -> readFile (head args)
    _ -> putStrLn "Usage error" >> exitWith (ExitFailure 84) >> return ""
  runInterpreter input

runInterpreter :: String -> IO ()
runInterpreter input = 
  case Core.readProgram input of
    Left parseError -> 
      putStrLn ("Parse error: " ++ show parseError) >> exitWith (ExitFailure 84)
    Right expressions -> do
      result <- processExpressions initialEnv expressions
      case result of
        Left err -> putStrLn err >> exitWith (ExitFailure 84)
        Right (_finalEnv, finalValue) -> print finalValue

processExpressions :: Env -> [LispVal] -> IO (Either String (Env, LispVal))
processExpressions env [] = return $ Right (env, Bool True)
processExpressions env [expr] = 
  case Evaluator.eval env expr of
    Left err -> return $ Left err
    Right val -> return $ Right (env, val)
processExpressions env (expr:rest) = 
  case Evaluator.eval env expr of
    Left err -> return $ Left err
    Right val -> 
      case expr of
        List (Atom "define" : Atom var : _) ->
          processExpressions (defineVar var val env) rest
        _ -> 
          processExpressions env rest
