module AST.Helpers where

import AST.AST
import Text.Megaparsec.Pos (SourcePos)

getExprPos :: Expr -> SourcePos
getExprPos (BoolLit  (Located p _))     = p
getExprPos (NumLit   (Located p _))     = p
getExprPos (FloatLit (Located p _))     = p
getExprPos (Var      (Located p _))     = p
getExprPos (BinOp _  (Located p _)  _)  = p
getExprPos (Call     (Located p _)  _)  = p
