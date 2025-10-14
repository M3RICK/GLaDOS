module IR.Types where

-- | A complete compiled program
data IRProgram = IRProgram
  { functions :: [CompiledFunction]
  , mainIndex :: Int  -- Il est ou le main fredo ?
  } deriving (Show, Eq)

-- | A single compiled function
data CompiledFunction = CompiledFunction
  { funcName :: String
  , paramCount :: Int         -- How many parameters?
  , localVarCount :: Int      -- How many local variables?
  , code :: [Instruction]     -- The actual instructions
  } deriving (Show, Eq)

data Instruction
  -- push values onto the stack
  = PushInt Int
  | PushBool Bool
  | PushFloat Double

  -- local variables
  | GetLocal Int
  | SetLocal Int

  -- Arithmetic (Int)
  | AddInt
  | SubInt
  | MulInt
  | DivInt

  -- Arithmetic (Float)
  | AddFloat
  | SubFloat
  | MulFloat
  | DivFloat

  -- Unary operations
  | NegInt
  | NegFloat
  | NotBool

  -- Comparison (Int)
  | EqInt
  | NeqInt
  | LtInt
  | GtInt
  | LeInt
  | GeInt

  -- Comparison (Float)
  | EqFloat
  | NeqFloat
  | LtFloat
  | GtFloat
  | LeFloat
  | GeFloat

  -- Logical
  | AndBool
  | OrBool

  -- Control flow
  | Jump Int
  | JumpIfFalse Int
  | Call Int
  | Return

  -- Utility
  | Pop -- Discard top of stack
  | Halt -- End program
  deriving (Show, Eq)

-- value during compilation, for tracking purposes
data Value
  = VInt Int
  | VBool Bool
  | VFloat Double
  deriving (Show, Eq)
