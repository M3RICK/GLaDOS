module Parser.Statement (pStatement) where

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
  , pFor
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

pFor :: Parser Statement
pFor = do
  void (symbol "for")
  void (symbol "(")
  initStmt <- optional parseForInit
  void semi
  condition <- optional pExpr
  void semi
  update <- optional parseForUpdate
  void (symbol ")")
  body <- braces (many pStatement)
  return (For initStmt condition update body)

parseForInit :: Parser Statement
parseForInit = try pDeclNoSemi <|> try pAssignNoSemi
  where
    pDeclNoSemi = do
      t <- pType
      Located _ name <- pIdentifier
      initVal <- optional (symbol "=" *> pExpr)
      return (Decl t name initVal)
    pAssignNoSemi = do
      Located _ name <- pIdentifier
      void (symbol "=")
      expr <- pExpr
      return (Assign name expr)

parseForUpdate :: Parser Statement
parseForUpdate = do
  Located _ name <- pIdentifier
  void (symbol "=")
  expr <- pExpr
  return (Assign name expr)
