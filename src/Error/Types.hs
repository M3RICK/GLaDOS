module Error.Types
  ( CompilerError(..)
  , formatCompilerError
  , wrapParseError
  , wrapTypeErrors
  , wrapRuntimeError
  ) where

import Security.Types (TypeError)
import Security.ErrorFormat (formatError, formatTypeErrors)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)
import Text.Megaparsec.Pos (SourcePos)
import Data.Void (Void)

data CompilerError
  = ParseError String
  | TypeErrors [TypeError]
  | CompileError String SourcePos
  | RuntimeError String
  deriving (Show, Eq)

formatCompilerError :: CompilerError -> String
formatCompilerError (ParseError msg) = msg
formatCompilerError (TypeErrors errs) = formatTypeErrors errs
formatCompilerError (CompileError msg pos) =
  "Compilation error at " ++ show pos ++ ": " ++ msg
formatCompilerError (RuntimeError msg) = "Runtime error: " ++ msg

-- Convert a Megaparsec ParseErrorBundle to CompilerError
wrapParseError :: ParseErrorBundle String Void -> CompilerError
wrapParseError bundle = ParseError (errorBundlePretty bundle)

-- Convert Security module TypeError list to CompilerError
wrapTypeErrors :: [TypeError] -> CompilerError
wrapTypeErrors = TypeErrors

-- Convert VM runtime error string to CompilerError
wrapRuntimeError :: String -> CompilerError
wrapRuntimeError = RuntimeError
