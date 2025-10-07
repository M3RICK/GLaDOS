-- src/AST/AST.hs
module AST.AST where

import Text.Megaparsec.Pos (SourcePos)

-- | A value tagged with its source position (for nice error messages).
data Located a = Located
  { pos   :: SourcePos
  , value :: a
  } deriving (Eq, Show)

-- | Language types (we’ll extend later with Float/Char/String).
data Type
  = TypeInt
  | TypeBool
  | TypeVoid
  deriving (Eq, Show)

-- | Binary operators.
data Op
  = Add | Sub | Mul | Div
  | Eq  | Neq | Lt | Gt | Le | Ge
  | And | Or
  deriving (Eq, Show)

-- | Expressions.
data Expr
  = BoolLit (Located Bool)
  | NumLit  (Located Int)
  | Var     (Located String)
  | BinOp   Op (Located Expr) (Located Expr)
  | Call    (Located String) [Expr]
  deriving (Eq, Show)

-- | Statements.
data Statement
  = Decl   Type String (Maybe Expr)             -- int x; / int x = expr;
  | Assign String Expr                          -- x = expr;
  | If     Expr [Statement] (Maybe [Statement]) -- if (...) { ... } else { ... }
  | While  Expr [Statement]                     -- while (...) { ... }
  | Return Expr                                 -- return expr;
  | ExprStmt Expr                               -- e.g. a function call as stmt
  deriving (Eq, Show)

-- | Function parameter.
data Parameter = Parameter
  { paramType :: Type
  , paramName :: String
  } deriving (Eq, Show)

-- | Function definition.
data Function = Function
  { fType   :: Type
  , fName   :: String
  , fParams :: [Parameter]
  , fBody   :: [Statement]
  } deriving (Eq, Show)

-- | Program = list of functions.
newtype Program = Program [Function]
  deriving (Eq, Show)
