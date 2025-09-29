module Parser.Function where

import Parser.Lexer
import Parser.Statement  -- This import is fine now
import Parser.Types     -- Import pType from here
import AST.AST

import Text.Megaparsec
import Control.Monad (void)

pFunction :: Parser Function
pFunction = do
  t <- pType
  name <- pIdentifier
  params <- parens (pParameter `sepBy` comma)
  body <- braces (many pStatement)
  return (Function t name params body)

pParameter :: Parser Parameter
pParameter = do
  t <- pType
  name <- pIdentifier
  return (Parameter { paramType = t, paramName = name })
