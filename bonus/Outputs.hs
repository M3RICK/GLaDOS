module Outputs (Value(..), Instr(..), exec) where

import System.IO (hPutStrLn, stderr)

data Value = VInt Int
  | VBool Bool
  | VString String
  deriving (Eq, Show)

data Instr = Print Value
  | Printerror Value
  | Return Value
  deriving (Eq, Show)

exec :: [Instr] -> IO ()
exec [] = return ()
exec (instr:rest) =
  case instr of
    Print v -> do
      putStrLn (showV v)
      exec rest
    Printerror v -> do
      hPutStrLn stderr (showV v)
      exec rest
    Return v -> putStrLn (showV v)

showV :: Value -> String
showV (VInt n) = show n
showV (VBool b) = if b then "#t" else "#f"
showV (VString s) = s
