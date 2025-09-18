module Main (main) where
import System.Environment (getArgs)
import System.Exit
import Types
import Builtins
import qualified Parser.Core as Core
import qualified Evaluator.Core as Evaluator

main :: IO ()
main = do
  args <- getArgs
  input <- case length args of
    0 -> getContents
    1 -> readFile (head args)
    _ -> putStrLn "Usage: ./glados [file]" >> exitWith (ExitFailure 84) >> return ""
  runInterpreter input

runInterpreter :: String -> IO ()
runInterpreter input =
  case Core.readProgram input of
    Left parseError -> do
      putStrLn $ "Parse error: " ++ show parseError
      exitWith (ExitFailure 84)
    Right expressions -> do
      result <- runProgram initialEnv expressions
      case result of
        Left err -> putStrLn err >> exitWith (ExitFailure 84)
        Right (_finalEnv, finalValue) -> print finalValue

runProgram :: Env -> [LispVal] -> IO (Either String (Env, LispVal))
runProgram env [] = return $ Right (env, Bool True)
runProgram env (expr:rest) =
  case Evaluator.eval env expr of
    Left err -> return $ Left err
    Right (env', val) ->
      if null rest
        then return $ Right (env', val)
        else runProgram env' rest