module AST.Helpers (getExprPos, defaultPos, extractFunctions) where

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

-- Extract only function definitions from top-level declarations
extractFunctions :: [TopLevel] -> [Function]
extractFunctions = foldr extractFunc []
  where
    extractFunc (FuncDef f) acc = f : acc
    extractFunc (FuncProto _) acc = acc
