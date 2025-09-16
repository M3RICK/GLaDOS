module Types where

data LispVal
  = Atom String
  | Number Integer
  | Bool Bool
  | List [LispVal]
  deriving (Show, Eq)
