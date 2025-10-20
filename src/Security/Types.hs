module Security.Types
  ( VarEnv
  , FuncEnv
  , InitializedVars
  , CheckEnv(..)
  , TypeError(..)
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import AST.AST
import Text.Megaparsec.Pos (SourcePos)

-- Variable environment
type VarEnv = M.Map String Type

-- Function environment: maps function names to (return type, param types)
type FuncEnv = M.Map String (Type, [Type])

-- Set of initialized variables
type InitializedVars = S.Set String

-- Combined environment for type checking
data CheckEnv = CheckEnv
  { varEnv  :: VarEnv
  , funcEnv :: FuncEnv
  , currentReturnType :: Maybe Type
  , initializedVars :: InitializedVars
  } deriving (Show, Eq)

-- Type errors
data TypeError
  = UndefinedVar String SourcePos
  | UndefinedFunc String SourcePos
  | TypeMismatch Type Type SourcePos String  -- expected, got, position, context
  | AlreadyDefined String SourcePos
  | WrongArgCount String Int Int SourcePos  -- funcName, expectedCount, gotCount, position
  | ReturnTypeMismatch Type Type SourcePos
  | UninitializedVar String SourcePos
  | MissingReturn String
  | DivisionByZero SourcePos
  deriving (Show, Eq)
