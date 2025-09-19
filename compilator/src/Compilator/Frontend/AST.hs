module Compilator.Frontend.AST where

type Name = String

data Program = Program [Top]            deriving (Show, Eq)
data Top     = TDecl Decl | TStmt Stmt  deriving (Show, Eq)

data Decl
  = DLet Name Expr
  | DFunc Name [Name] Expr
  deriving (Show, Eq)

data Stmt
  = SPrint Expr
  deriving (Show, Eq)

data Expr
  = ELitInt  Integer
  | EVar     Name
  | ECall    Name [Expr]
  | EBin     BinOp Expr Expr
  deriving (Show, Eq)

-- Minimal starter: seulement + et *
data BinOp = Add | Mul
  deriving (Show, Eq)
