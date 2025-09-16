module Types where

data LispVal
  = Atom String
  | Bool Bool
  | Number Integer
  | List [LispVal]
  deriving (Show, Eq)
