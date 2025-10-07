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

  -- local variables
  | GetLocal Int
  | SetLocal Int

  -- Arithmetic
  | AddInt
  | SubInt
  | MulInt
  | DivInt

  -- Comparison
  | EqInt
  | NeqInt
  | LtInt
  | GtInt
  | LeInt
  | GeInt

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
  deriving (Show, Eq)
