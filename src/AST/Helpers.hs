module AST.Helpers (getExprPos, defaultPos) where

import AST.AST
import Text.Megaparsec.Pos (SourcePos, initialPos)

-- Extract position from expressions
getExprPos :: Expr -> SourcePos
getExprPos (BoolLit (Located p _)) = p
getExprPos (NumLit (Located p _)) = p
getExprPos (FloatLit (Located p _)) = p
getExprPos (Var (Located p _)) = p
getExprPos (BinOp _ (Located p _) _) = p
getExprPos (UnOp _ (Located p _)) = p
getExprPos (Call (Located p _) _) = p

-- Default position when we can't determine one
defaultPos :: SourcePos
defaultPos = initialPos "unknown"
