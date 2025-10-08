##
## EPITECH PROJECT, 2025
## G-FUN-500-TLS-5-1-glados-2
## File description:
## Outputs.hs
##

module Outputs (Value(..), Instr(..), exec) where

import System.IO (hPutStrLn, stderr)

data Value = VInt Int
  | VBool Bool
  | VString String
  deriving (Eq, Show)

data Instr = Push Value
  | Addition
  | Subtraction
  | Multiplication
  | Division
  | Print
  | Printerror
  | Read
  | Return
  deriving (Eq, Show)

type Stack = [Value]

exec :: [Instr] -> Stack -> IO (Either String Value)
exec [] stack =
  case stack of
    (v:_) -> return (Right v)
    []    -> return (Left "error: empty stack on termination")
exec (instr:rest) stack =
  case instr of
    Push v ->
      exec rest (v : stack)
    Addition ->
      case stack of
        (VInt a : VInt b : s) -> exec rest (VInt (b + a) : s)
        _ -> return (Left "error: addition needs two integers")
    Subtraction ->
      case stack of
        (VInt a : VInt b : s) -> exec rest (VInt (b - a) : s)
        _ -> return (Left "error: substraction needs two integers")
    Multiplication ->
      case stack of
        (VInt a : VInt b : s) -> exec rest (VInt (b * a) : s)
        _ -> return (Left "error: multiplication needs two integers")
    Division ->
      case stack of
        (VInt 0 : _) -> return (Left "error: division by zero")
        (VInt a : VInt b : s) -> exec rest (VInt (b `div` a) : s)
        _ -> return (Left "error: division needs two integers")
    Print ->
      case stack of
        (v:s) -> do
          print v
          exec rest s
        [] -> return (Left "error: print on empty stack")
    Printerror ->
      case stack of
        (v:s) -> do
          hPutStrLn stderr (show v)
          exec rest s
        [] -> return (Left "error: empty stack")
    Read -> do
      line <- getLine
      exec rest (VString line : stack)
    Return ->
      case stack of
        (v:_) -> return (Right v)
        [] -> return (Left "error: empty stack return")
