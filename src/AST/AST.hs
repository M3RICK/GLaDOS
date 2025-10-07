module AST.AST where

import Text.Megaparsec.Pos (SourcePos)

-- Program
data Program = Program [Function]
  deriving (Show, Eq)

-- Function
data Function = Function
  { fType   :: Type
  , fName   :: String
  , fParams :: [Parameter]
  , fBody   :: [Statement]
  }
  deriving (Show, Eq)

-- Parameter
data Parameter = Parameter
  { paramType :: Type
  , paramName :: String
  }
  deriving (Show, Eq)

-- Statements
data Statement
  = Decl Type String (Maybe Expr)
  | Assign String Expr
  | If Expr [Statement] (Maybe [Statement])
  | While Expr [Statement]
  | Return Expr
  | ExprStmt Expr
  deriving (Show, Eq)

-- Expressions
data Expr
  = BoolLit  (Located Bool)
  | NumLit   (Located Int)
  | FloatLit (Located Double)
  | Var      (Located String)
  | BinOp    Op (Located Expr) (Located Expr)
  | Call     (Located String) [Expr]
  deriving (Show, Eq)

-- Types
data Type
  = TypeInt
  | TypeBool
  | TypeFloat
  | TypeVoid
  deriving (Show, Eq)

-- Operators
data Op
  = Add | Sub | Mul | Div
  | Eq | Neq | Lt | Gt | Le | Ge
  | And | Or
  deriving (Show, Eq)

-- With source position
data Located a = Located
  { pos   :: SourcePos
  , value :: a
  } deriving (Show, Eq)
