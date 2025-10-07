module VM.HelperFunc where

import IR.Types
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

-- Get a local variable by index
getLocal :: Int -> VMState -> VMResult Value
getLocal idx state
    | idx < 0 = Left $ "Invalid local index: " ++ show idx ++ " (negative)"
    | idx >= length (locals state) = Left $ "Local index out of bounds: " ++ show idx ++ " (max: " ++ show (length (locals state) - 1) ++ ")"
    | otherwise = Right (locals state !! idx)

-- Set a local variable by index
setLocal :: Int -> Value -> VMState -> VMResult VMState
setLocal idx val state
    | idx < 0 = Left $ "Invalid local index: " ++ show idx ++ " (negative)"
    | idx >= length (locals state) = Left $ "Local index out of bounds: " ++ show idx ++ " (max: " ++ show (length (locals state) - 1) ++ ")"
    | otherwise = Right state {locals = updateAt idx val (locals state)}
    where
        updateAt :: Int -> a -> [a] -> [a]
        updateAt n item ls = take n ls ++ [item] ++ drop (n + 1) ls

-- Pop and type-check for Int
popInt :: VMState -> VMResult (Int, VMState)
popInt state = case pop state of
    Right (VInt n, newState) -> Right (n, newState)
    Right (VBool _, _) -> Left "Type error: expected Int, got Bool"
    Left err -> Left err

-- Pop and type-check for Bool
popBool :: VMState -> VMResult (Bool, VMState)
popBool state = case pop state of
    Right (VBool b, newState) -> Right (b, newState)
    Right (VInt _, _) -> Left "Type error: expected Bool, got Int"
    Left err -> Left err

-- BinOp does as follows pop 2 ints, apply op, push result
binaryIntOp :: (Int -> Int -> Int) -> VMState -> VMResult VMState
binaryIntOp op state =
    case popInt state of
        Left err -> Left err
        Right (b, state1) -> case popInt state1 of
            Left err -> Left err
            Right (a, state2) -> Right (push (VInt (a `op` b)) state2)

-- Same execution principle as above
compareInts :: (Int -> Int -> Bool) -> VMState -> VMResult VMState
compareInts cmp state =
    case popInt state of
        Left err -> Left err
        Right (b, state1) -> case popInt state1 of
            Left err -> Left err
            Right (a, state2) -> Right (push (VBool (a `cmp` b)) state2)

-- Just like BinOp
binaryBoolOp :: (Bool -> Bool -> Bool) -> VMState -> VMResult VMState
binaryBoolOp op state =
    case popBool state of
        Left err -> Left err
        Right (b, state1) -> case popBool state1 of
            Left err -> Left err
            Right (a, state2) -> Right (push (VBool (a `op` b)) state2)
