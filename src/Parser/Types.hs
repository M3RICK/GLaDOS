module Parser.Types (pType) where

import Parser.Lexer
import AST.AST
import Text.Megaparsec

-- Type parser (used by both Function and Statement)
pType :: Parser Type
pType =
      (TypeInt  <$ symbol "int")
  <|> (TypeFloat <$ symbol "float")
  <|> (TypeBool <$ symbol "bool")
  <|> (TypeVoid <$ symbol "void")
  <|> (TypeInfer <$ symbol "var")
