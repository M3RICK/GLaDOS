module Parser.Statement where

import Parser.Lexer
import Parser.Expr
import Parser.Types
import AST.AST

import Text.Megaparsec
import Control.Monad (void)

optionalSemi :: Parser ()
optionalSemi = optional semi >> return ()

pStatement :: Parser Statement
pStatement = choice
  [ pIf
  , pWhile
  , pReturn
  , pDecl
  , try pAssign  -- try because it starts like ExprStmt
  , pExprStmt    -- Must be last since it's the most general
  ]

pDecl :: Parser Statement
pDecl = do
  t <- pType
  Located _ name <- pIdentifier  -- Extract string from Located
  initVal <- optional (symbol "=" *> pExpr)
  optionalSemi
  return (Decl t name initVal)

pAssign :: Parser Statement
pAssign = do
  Located _ name <- pIdentifier  -- Extract string from Located
  void (symbol "=")
  expr <- pExpr
  optionalSemi
  return (Assign name expr)

pReturn :: Parser Statement
pReturn = do
  void (symbol "return")
  expr <- pExpr
  optionalSemi
  return (Return expr)

pExprStmt :: Parser Statement
pExprStmt = do
  expr <- pExpr
  optionalSemi
  return (ExprStmt expr)

pIf :: Parser Statement
pIf = do
  void (symbol "if")
  cond <- parens pExpr
  thn <- braces (many pStatement)
  els <- optional (symbol "else" *> braces (many pStatement))
  return (If cond thn els)

pWhile :: Parser Statement
pWhile = do
  void (symbol "while")
  cond <- parens pExpr
  body <- braces (many pStatement)
  return (While cond body)
