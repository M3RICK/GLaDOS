module Parser.Function where

import Parser.Lexer
import Parser.Statement
import Parser.Types
import AST.AST

import Text.Megaparsec
import Control.Monad (void)

pFunction :: Parser Function
pFunction = do
  t <- pType
  Located _ name <- pIdentifier  -- Extract the string from Located
  params <- parens (pParameter `sepBy` comma)
  body <- braces (many pStatement)
  return (Function t name params body)

pParameter :: Parser Parameter
pParameter = do
  t <- pType
  Located _ name <- pIdentifier  -- Extract the string from Located
  return (Parameter { paramType = t, paramName = name })
