module Parser.Types where

import AST.AST
import Parser.Lexer (Parser)

import Text.Megaparsec (choice, notFollowedBy, try)
import Text.Megaparsec.Char (alphaNumChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C

-- local space consumer (kept here so we don't depend on Lexer exports)
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

-- reserved keyword with word boundary, then trailing space/comments
reserved :: String -> Parser ()
reserved w = L.lexeme sc (try (C.string w *> notFollowedBy alphaNumChar))

-- Parse a type keyword
pType :: Parser Type
pType = choice
  [ reserved "int"   >> pure TypeInt
  , reserved "bool"  >> pure TypeBool
  , reserved "void"  >> pure TypeVoid
  , reserved "float" >> pure TypeFloat
  ]
