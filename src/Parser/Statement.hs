module Parser.Statement where

import Parser.Lexer
import Parser.Expr
import AST.AST

import Text.Megaparsec
import Text.Megaparsec.Char (eol)
import Control.Monad (void)


-- Optional semicolon helper function
------------------------------------------------------------

endStmt :: Parser()
endStmt = void semi <|> void (lexeme eol)

-- If / Else
------------------------------------------------------------

pIf :: Parser Statement
pIf = do
  void (symbol "if")
  cond <- parens pExpr
  thn <- braces (many pStatement)
  els <- optional (symbol "else" *> braces (many pStatement))
  return (If cond thn els)

-- While loop
------------------------------------------------------------

pWhile :: Parser Statement
pWhile = do
  void (symbol "while")
  cond <- parens pExpr
  body <- braces (many pStatement)
  return (While cond body)


-- Statement parser
------------------------------------------------------------

pStatement :: Parser Statement
pStatement =
      try pDecl
  <|> try pAssign
  <|> try pReturn
  <|> try pIf
  <|> try pWhile
  <|> ExprStmt <$> (pExpr <* endStmt)  -- catch function calls etc.

-- Declaration: int x; | int x = expr;
------------------------------------------------------------

pDecl :: Parser Statement
pDecl = do
  t <- pType
  name <- pIdentifier
  initVal <- optional (symbol "=" *> pExpr)
  endStmt
  return (Decl t name initVal)

-- Assignment: x = expr;
------------------------------------------------------------

pAssign :: Parser Statement
pAssign = do
  name <- pIdentifier
  void (symbol "=")
  expr <- pExpr
  endStmt
  return (Assign name expr)

-- Return return expr;
------------------------------------------------------------

pReturn :: Parser Statement
pReturn = do
  void (symbol "return")
  expr <- pExpr
  endStmt
  return (Return expr)
