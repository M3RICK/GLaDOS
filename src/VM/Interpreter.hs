module VM.Interpreter where

import IR.Types
import VM.HelperFunc

-- Stack management
executeStackOp :: Instruction -> VMState -> VMResult VMState
executeStackOp instr state = case instr of
    PushInt n -> Right (push (VInt n) state)
    PushBool b -> Right (push (VBool b) state)
    Pop -> case pop state of
        Right (_, newState) -> Right newState
        Left err -> Left err
    _ -> Left "Internal error: not a stack operation"

-- Local var interactuions
executeVarOp :: Instruction -> VMState -> VMResult VMState
executeVarOp instr state = case instr of
    GetLocal idx -> case getLocal idx state of
        Right val -> Right (push val state)
        Left err -> Left err
    SetLocal idx -> case pop state of
        Right (val, newState) -> setLocal idx val newState
        Left err -> Left err
    _ -> Left "Internal error: not a variable operation"

-- Math
executeArithOp :: Instruction -> VMState -> VMResult VMState
executeArithOp instr state = case instr of
    AddInt -> binaryIntOp (+) state
    SubInt -> binaryIntOp (-) state
    MulInt -> binaryIntOp (*) state
    DivInt -> case popInt state of
        Left err -> Left err
        Right (b, state1) ->
            if b == 0
            then Left "Runtime error: Division by zero"
            else case popInt state1 of
                Left err -> Left err
                Right (a, state2) -> Right (push (VInt (a `div` b)) state2)
    _ -> Left "Internal error: not an arithmetic operation"

-- Comparaison
executeCompOp :: Instruction -> VMState -> VMResult VMState
executeCompOp instr state = case instr of
    EqInt -> compareInts (==) state
    NeqInt -> compareInts (/=) state
    LtInt -> compareInts (<) state
    GtInt -> compareInts (>) state
    LeInt -> compareInts (<=) state
    GeInt -> compareInts (>=) state
    _ -> Left "Internal error: not a comparison operation"

-- &&/||
executeLogicOp :: Instruction -> VMState -> VMResult VMState
executeLogicOp instr state = case instr of
    AndBool -> binaryBoolOp (&&) state
    OrBool -> binaryBoolOp (||) state
    _ -> Left "Internal error: not a logical operation"

-- Flow op
executeControlFlow :: Instruction -> VMState -> VMResult VMState
executeControlFlow instr state = case instr of
    Jump addr -> Right (state {pc = addr})
    JumpIfFalse addr -> case popBool state of
        Left err -> Left err
        Right (condition, newState) ->
            if condition
            then Right newState
            else Right (newState {pc = addr})
    Call _funcIdx -> Left "Call instruction not yet implemented"
    Return -> Left "Return instruction not yet implemented"
    Halt -> Right state
    _ -> Left "Internal error: not a control flow operation"

-- The Pâté Carrefour
executeInstruction :: Instruction -> VMState -> VMResult VMState
executeInstruction instr state = case instr of
    PushInt _ -> executeStackOp instr state
    PushBool _ -> executeStackOp instr state
    Pop -> executeStackOp instr state
    GetLocal _ -> executeVarOp instr state
    SetLocal _ -> executeVarOp instr state
    AddInt -> executeArithOp instr state
    SubInt -> executeArithOp instr state
    MulInt -> executeArithOp instr state
    DivInt -> executeArithOp instr state
    EqInt -> executeCompOp instr state
    NeqInt -> executeCompOp instr state
    LtInt -> executeCompOp instr state
    GtInt -> executeCompOp instr state
    LeInt -> executeCompOp instr state
    GeInt -> executeCompOp instr state
    AndBool -> executeLogicOp instr state
    OrBool -> executeLogicOp instr state
    Jump _ -> executeControlFlow instr state
    JumpIfFalse _ -> executeControlFlow instr state
    Call _ -> executeControlFlow instr state
    Return -> executeControlFlow instr state
    Halt -> executeControlFlow instr state
