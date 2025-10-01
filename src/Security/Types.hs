module Security.Types where

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
  | TypeMismatch
      { expected :: Type
      , got :: Type
      , position :: SourcePos
      , context :: String
      }
  | AlreadyDefined String SourcePos
  | WrongArgCount
      { funcName :: String
      , expectedCount :: Int
      , gotCount :: Int
      , position :: SourcePos
      }
  | ReturnTypeMismatch Type Type SourcePos
  | UninitializedVar String SourcePos
  deriving (Show, Eq)
