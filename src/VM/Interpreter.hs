module VM.Interpreter (executeInstruction, execute) where

import IR.Types
import VM.HelperFunc
import VM.InstructionHandlers

-- The Pâté Carrefour (Main dispatcher)
executeInstruction :: IRProgram -> Instruction -> VMState -> VMResult VMState
executeInstruction program instr state = case instr of
    PushInt _ -> executeStackOp instr state
    PushBool _ -> executeStackOp instr state
    PushFloat _ -> executeStackOp instr state
    Pop -> executeStackOp instr state
    GetLocal _ -> executeVarOp instr state
    SetLocal _ -> executeVarOp instr state
    AddInt -> executeArithOp instr state
    SubInt -> executeArithOp instr state
    MulInt -> executeArithOp instr state
    DivInt -> executeArithOp instr state
    AddFloat -> executeArithOp instr state
    SubFloat -> executeArithOp instr state
    MulFloat -> executeArithOp instr state
    DivFloat -> executeArithOp instr state
    NegInt -> executeArithOp instr state
    NegFloat -> executeArithOp instr state
    NotBool -> executeLogicOp instr state
    EqInt -> executeCompOp instr state
    NeqInt -> executeCompOp instr state
    LtInt -> executeCompOp instr state
    GtInt -> executeCompOp instr state
    LeInt -> executeCompOp instr state
    GeInt -> executeCompOp instr state
    EqFloat -> executeCompOp instr state
    NeqFloat -> executeCompOp instr state
    LtFloat -> executeCompOp instr state
    GtFloat -> executeCompOp instr state
    LeFloat -> executeCompOp instr state
    GeFloat -> executeCompOp instr state
    AndBool -> executeLogicOp instr state
    OrBool -> executeLogicOp instr state
    Jump _ -> executeControlFlow execute program instr state
    JumpIfFalse _ -> executeControlFlow execute program instr state
    Call _ -> executeControlFlow execute program instr state
    Return -> executeControlFlow execute program instr state
    Halt -> executeControlFlow execute program instr state

-- Main execution loop should hopefully run a function until Halt or error
execute :: IRProgram -> Int -> [Value] -> VMResult Value
execute program funcIdx args =
    case getFunctionAt program funcIdx of
        Left err -> Left err
        Right func ->
            let initialLocals = args ++ replicate (localVarCount func - length args) (VInt 0)
                initialState = VMState {
                    stack = [],
                    locals = initialLocals,
                    pc = 0,
                    callStack = []
                }
            in runLoop program funcIdx func initialState

-- The main loop: fetch, execute, increment PC, repeat
runLoop :: IRProgram -> Int -> CompiledFunction -> VMState -> VMResult Value
runLoop program funcIdx func state
    | pc state < 0 = Left "PC became negative"
    | pc state >= length (code func) = Left "PC out of bounds"
    | otherwise =
        let instruction = code func !! pc state
            oldPC = pc state
        in case executeInstruction program instruction state of
            Left err -> Left err
            Right newState ->
                case instruction of
                    Halt -> case stack newState of
                        [] -> Left "Program halted with empty stack"
                        (result:_) -> Right result
                    Return -> case stack newState of
                        [] -> Left "Return with empty stack"
                        (result:_) -> Right result
                    _ -> if pc newState == oldPC
                         then runLoop program funcIdx func (newState {pc = pc newState + 1})
                         else runLoop program funcIdx func newState
