module Parser.FloatLit (pFloatLit) where

import AST.AST
import Parser.Lexer (Parser)
import Text.Megaparsec (getSourcePos, try)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (space1)
import Data.Scientific (toRealFloat, isInteger)

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

pFloatLit :: Parser Expr
pFloatLit = try $ do
  pos <- getSourcePos
  s   <- L.lexeme sc (L.signed sc L.scientific)
  if isInteger s
    then fail "not a float"
    else pure (FloatLit (Located pos (toRealFloat s)))
