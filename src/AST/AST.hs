module AST.AST (Program(..), Function(..), Parameter(..), Statement(..), Expr(..), Type(..), Op(..)) where

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
  { pType :: Type
  , pName :: String
  }
  deriving (Show, Eq)

-- | Statements inside a function
data Statement
  = Decl Type String (Maybe Expr)           -- int x; or int x = expr;
  | Assign String Expr                      -- x = expr;
  | If Expr [Statement] (Maybe [Statement]) -- if (...) { ... } else { ... }
  | While Expr [Statement]                  -- while (...) { ... }
  | Return Expr                             -- return expr;
  | ExprStmt Expr                           -- e.g. function call as statement
  deriving (Show, Eq)

-- | Expressions
data Expr
  = BoolLit Bool
  | NumLit Int
  | Var String
  | BinOp Op Expr Expr
  | Call String [Expr]      -- function calls, e.g. foo(1,2)
  deriving (Show, Eq)

-- | Supported types
data Type
  = TypeInt
  | TypeBool
  | TypeVoid
  deriving (Show, Eq)

-- | Operators
data Op
  = Add | Sub | Mul | Div
  | Eq | Neq | Lt | Gt | Le | Ge
  | And | Or
  deriving (Show, Eq)