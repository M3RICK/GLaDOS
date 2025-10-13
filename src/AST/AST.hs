module AST.AST where

import Text.Megaparsec.Pos (SourcePos)

-- | A whole program = list of functions
data Program = Program [Function]
  deriving (Show, Eq)

-- | Function declaration
data Function = Function
  { fType       :: Type
  , fName       :: String
  , fParams     :: [Parameter]
  , fBody       :: [Statement]
  }
  deriving (Show, Eq) -- temporaire va falloir creer un PrintAst :: AST -> String et aussi utiliser SourcePos de Megaparsec pour des beaux msg d erreurs

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
  | Var (Located String)
  | BinOp Op (Located Expr) (Located Expr)  -- Keep position of whole expression
  | Call (Located String) [Expr]
  deriving (Show, Eq)

-- | Supported types
data Type
  = TypeInt
  | TypeBool
  | TypeVoid
  | TypeInfer
  deriving (Show, Eq)

-- | Operators
data Op
  = Add | Sub | Mul | Div
  | Eq | Neq | Lt | Gt | Le | Ge
  | And | Or
  deriving (Show, Eq)

-- Security
data Located a
    = Located {
        pos :: SourcePos,
        value :: a
    } deriving (Show, Eq)
