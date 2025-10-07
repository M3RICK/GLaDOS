module VM.Interpreter where

import IR.Types
import Control.Monad (when)
type VMError = String -- error msg string
type VMResult a = Either VMError a

-- runtime state
data VMState = VMState
    { stack :: [Value] -- Stack... holds vars and eventual results (Vram)
    , locals :: [Value] -- Local variables (params + local vars, indexed from 0)
    , pc :: Int -- Execution Index
    , callStack :: [CallFrame] -- Info saved during function call (restored on return(hopefully ;-;))
    }
    deriving (Show)

-- Temp info for func call
data CallFrame = CallFrame
    { returnPC :: Int -- Next exec index
    , returnFuncIdx :: Int -- Which function to return to
    , savedLocals :: [Value] -- Previous positions local variables, basically restores old info after func exex
    }
    deriving (Show)

-- Push a value on curr stack
push :: Value -> VMState -> VMState
push val state = state {stack = val : stack state}

-- Pop a value from the stack (returns error if empty)
pop :: VMState -> VMResult (Value, VMState)
pop state = case stack state of
    []     -> Left "Stack underflow: tried to pop from empty stack"
    (x:xs) -> Right (x, state {stack = xs})
