module VM.InstructionHandlers (
    executeStackOp
  , executeVarOp
  , executeArithOp
  , executeCompOp
  , executeLogicOp
  , executeControlFlow
  , getFunctionAt
) where

import IR.Types
import VM.HelperFunc

-- Stack management
executeStackOp :: Instruction -> VMState -> VMResult VMState
executeStackOp instr state = case instr of
    PushInt n -> Right (push (VInt n) state)
    PushBool b -> Right (push (VBool b) state)
    PushFloat f -> Right (push (VFloat f) state)
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
    AddFloat -> binaryFloatOp (+) state
    SubFloat -> binaryFloatOp (-) state
    MulFloat -> binaryFloatOp (*) state
    DivFloat -> case popFloat state of
        Left err -> Left err
        Right (b, state1) ->
            if b == 0.0
            then Left "Runtime error: Division by zero"
            else case popFloat state1 of
                Left err -> Left err
                Right (a, state2) -> Right (push (VFloat (a / b)) state2)
    NegInt -> case popInt state of
        Left err -> Left err
        Right (n, newState) -> Right (push (VInt (-n)) newState)
    NegFloat -> case popFloat state of
        Left err -> Left err
        Right (n, newState) -> Right (push (VFloat (-n)) newState)
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
    EqFloat -> compareFloats (==) state
    NeqFloat -> compareFloats (/=) state
    LtFloat -> compareFloats (<) state
    GtFloat -> compareFloats (>) state
    LeFloat -> compareFloats (<=) state
    GeFloat -> compareFloats (>=) state
    _ -> Left "Internal error: not a comparison operation"

-- &&/||
executeLogicOp :: Instruction -> VMState -> VMResult VMState
executeLogicOp instr state = case instr of
    AndBool -> binaryBoolOp (&&) state
    OrBool -> binaryBoolOp (||) state
    NotBool -> case popBool state of
        Left err -> Left err
        Right (b, newState) -> Right (push (VBool (not b)) newState)
    _ -> Left "Internal error: not a logical operation"

-- Flow op
-- VMExecutor is a alias is Helper dummy
executeControlFlow :: VMExecutor -> IRProgram -> Instruction -> VMState -> VMResult VMState
executeControlFlow _ _ (Jump addr) state = Right (state {pc = pc state + addr})
executeControlFlow _ _ (JumpIfFalse addr) state = case popBool state of
    Left err -> Left err
    Right (condition, newState) ->
        if condition
        then Right newState
        else Right (newState {pc = pc newState + addr})
executeControlFlow executeFunc program (Call funcIdx) state =
    case getFunctionAt program funcIdx of
        Left err -> Left err
        Right targetFunc ->
            let argCount = paramCount targetFunc
            in case popNValues argCount [] state of
                Left err -> Left err
                Right (args, stateAfterPop) ->
                    case executeFunc program funcIdx args of
                        Left err -> Left err
                        Right result ->
                            Right $ push result stateAfterPop
executeControlFlow _ _ Return state = Right state
executeControlFlow _ _ Halt state = Right state
executeControlFlow _ _ _ _ = Left "Internal error: not a control flow operation"

-- Helper to get function from program
-- Had to move him back here because of circular import error thingy
getFunctionAt :: IRProgram -> Int -> VMResult CompiledFunction
getFunctionAt program idx
    | idx < 0 = Left $ "Invalid function index: " ++ show idx ++ " (negative)"
    | idx >= length (functions program) = Left $ "Function index out of bounds: " ++ show idx
    | otherwise = Right (functions program !! idx)
