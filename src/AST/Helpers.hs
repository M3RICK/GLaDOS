module AST.Helpers (getExprPos, defaultPos) where

import AST.AST
import Text.Megaparsec.Pos (SourcePos, initialPos)

-- Extract position from expressions
getExprPos :: Expr -> SourcePos
getExprPos (BoolLit (Located pos _)) = pos
getExprPos (NumLit (Located pos _)) = pos
getExprPos (FloatLit (Located pos _)) = pos
getExprPos (Var (Located pos _)) = pos
getExprPos (BinOp _ (Located pos _) _) = pos
getExprPos (UnOp _ (Located pos _)) = pos
getExprPos (Call (Located pos _) _) = pos

-- Default position when we can't determine one
defaultPos :: SourcePos
defaultPos = initialPos "unknown"
