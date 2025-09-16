module Types where
import qualified Data.Map as Map

data LispVal
  = Atom String
  | Bool Bool
  | Number Integer
  | List [LispVal]
  | Function [String] LispVal Env
  | Builtin ([LispVal] -> Either String LispVal)
  deriving (Show, Eq)

type Env = Map String LispVal