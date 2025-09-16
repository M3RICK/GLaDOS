module Types where
import qualified Data.Map as Map

data LispVal
  = Atom String
  | Bool Bool
  | Number Integer
  | List [LispVal]
  | Function [String] LispVal Env
  | Builtin ([LispVal] -> Either String LispVal)
  --deriving (Show, Eq)

-- custom Show
instance Show LispVal where
  show (Atom s)        = s
  show (Bool True)     = "#t"
  show (Bool False)    = "#f"
  show (Number n)      = show n
  show (List xs)       = "(" ++ unwords (map show xs) ++ ")"
  show (Function _ _ _) = "<function>"
  show (Builtin _)      = "<builtin>"

-- custom Eq
instance Eq LispVal where
  (Atom a)      == (Atom b)      = a == b
  (Bool a)      == (Bool b)      = a == b
  (Number a)    == (Number b)    = a == b
  (List a)      == (List b)      = a == b
  -- Functions and Builtins cannot be compared
  _             == _             = False

type Env = Map.Map String LispVal
