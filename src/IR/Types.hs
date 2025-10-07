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
  -- Stack operations: push values onto the stack
  = PushInt Int              -- Push integer literal
  | PushBool Bool            -- Push boolean literal

  -- Variable operations: work with local variables
  | GetLocal Int             -- Load local variable N onto stack
  | SetLocal Int             -- Store top of stack into local variable N

  -- Arithmetic: pop two values, push result
  | AddInt                   -- a + b
  | SubInt                   -- a - b
  | MulInt                   -- a * b
  | DivInt                   -- a / b

  -- Comparison: pop two values, push bool result
  | EqInt                    -- a == b
  | NeqInt                   -- a != b
  | LtInt                    -- a < b
  | GtInt                    -- a > b
  | LeInt                    -- a <= b
  | GeInt                    -- a >= b

  -- Logical: pop two bools, push bool result
  | AndBool                  -- a && b
  | OrBool                   -- a || b

  -- Control flow
  | Jump Int                 -- Unconditional jump to instruction N
  | JumpIfFalse Int          -- Jump to instruction N if top of stack is false
  | Call Int                 -- Call function N
  | Return                   -- Return from current function

  -- Utility
  | Pop                      -- Discard top of stack
  | Halt                     -- End program
  deriving (Show, Eq)

-- | Represents a value during compilation (for tracking types)
data Value
  = VInt Int
  | VBool Bool
  deriving (Show, Eq)
