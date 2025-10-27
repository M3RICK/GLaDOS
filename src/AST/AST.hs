module AST.AST (
    Program(..)
  , TopLevel(..)
  , Function(..)
  , FunctionDecl(..)
  , Parameter(..)
  , Statement(..)
  , Expr(..)
  , Type(..)
  , Op(..)
  , UnOp(..)
  , Located(..)
) where

import Text.Megaparsec.Pos (SourcePos)

-- | A whole program = list of top-level declarations
data Program = Program [TopLevel]
  deriving (Show, Eq)

-- Ca peut etre soit une fonnction ou une definition ou un prototype
data TopLevel
  = FuncDef Function          -- int foo(int x) { ... }
  | FuncProto FunctionDecl    -- int foo(int x);
  deriving (Show, Eq)

-- | Fonction definition avec ce que ca contient
data Function = Function
  { fType       :: Type
  , fName       :: String
  , fParams     :: [Parameter]
  , fBody       :: [Statement]
  }
  deriving (Show, Eq)

-- | Function declaration/prototype (no body)
data FunctionDecl = FunctionDecl
  { fdType   :: Type
  , fdName   :: String
  , fdParams :: [Parameter]
  }
  deriving (Show, Eq)

-- | Function parameter
data Parameter = Parameter
  { paramType :: Type
  , paramName :: String
  }
  deriving (Show, Eq)

-- | Statements inside a function
data Statement
  = Decl Type String (Maybe Expr)           -- int x; or int x = expr;
  | Assign String Expr                      -- x = expr;
  | If Expr [Statement] (Maybe [Statement]) -- if (...) { ... } else { ... }
  | While Expr [Statement]                  -- while (...) { ... }
  | For (Maybe Statement) (Maybe Expr) (Maybe Statement) [Statement]
  | Return Expr                             -- return expr;
  | ExprStmt Expr                           -- e.g. function call as statement
  deriving (Show, Eq)

-- | Expressions
data Expr
  = BoolLit (Located Bool)
  | NumLit (Located Int)
  | FloatLit (Located Double)
  | Var (Located String)
  | BinOp Op (Located Expr) (Located Expr)  -- Keep position of whole expression
  | UnOp UnOp (Located Expr)                -- Unary operations
  | Call (Located String) [Expr]
  deriving (Show, Eq)

-- | Supported types
data Type
  = TypeInt
  | TypeFloat
  | TypeBool
  | TypeVoid
  | TypeInfer
  deriving (Show, Eq)

-- operators
data Op
  = Add | Sub | Mul | Div
  | Eq | Neq | Lt | Gt | Le | Ge
  | And | Or
  deriving (Show, Eq)

-- Unary Operators
data UnOp
  = Neg  -- Negation (-)
  | Not  -- Logical not (!)
  deriving (Show, Eq)

-- Security
data Located a
    = Located {
        pos :: SourcePos,
        value :: a
    } deriving (Show, Eq)
