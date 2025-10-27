module Parser.Function (pTopLevel, pFunction, pFunctionDecl) where

import Parser.Lexer
import Parser.Statement
import Parser.Types
import AST.AST

import Text.Megaparsec

pTopLevel :: Parser TopLevel
pTopLevel = try pFunctionDefinition <|> pPrototype

pFunctionDefinition :: Parser TopLevel
pFunctionDefinition = do
  t <- pType
  Located _ name <- pIdentifier
  params <- parens (pParameter `sepBy` comma)
  body <- braces (many pStatement)
  return (FuncDef (Function t name params body))

-- Prototype = function sans body
pPrototype :: Parser TopLevel
pPrototype = do
  t <- pType
  Located _ name <- pIdentifier
  params <- parens (pParameter `sepBy` comma)
  _ <- semi
  return (FuncProto (FunctionDecl t name params))

pFunction :: Parser Function
pFunction = do
  t <- pType
  Located _ name <- pIdentifier
  params <- parens (pParameter `sepBy` comma)
  body <- braces (many pStatement)
  return (Function t name params body)

pFunctionDecl :: Parser FunctionDecl
pFunctionDecl = do
  t <- pType
  Located _ name <- pIdentifier
  params <- parens (pParameter `sepBy` comma)
  _ <- semi
  return (FunctionDecl t name params)

pParameter :: Parser Parameter
pParameter = do
  t <- pType
  Located _ name <- pIdentifier  -- Extract the string from Located
  return (Parameter { paramType = t, paramName = name })
