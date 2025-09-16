module Types where

import qualified Data.Map as Map

data LispVal
  = Atom String
  | Bool Bool
  | Number Integer
  | List [LispVal]
  | Function [String] LispVal Env
  | Builtin ([LispVal] -> Either String LispVal)

type Env = Map.Map String LispVal

-- Manual Show instance
instance Show LispVal where
  show (Atom s) = s
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Number n) = show n
  show (List vals) = "(" ++ unwords (map show vals) ++ ")"
  show (Function params _ _) = "#<procedure:" ++ unwords params ++ ">"
  show (Builtin _) = "#<builtin>"

-- Manual Eq instance
instance Eq LispVal where
  (Atom a) == (Atom b) = a == b
  (Bool a) == (Bool b) = a == b
  (Number a) == (Number b) = a == b
  (List a) == (List b) = a == b
  _ == _ = False  -- Functions and builtins are never equal