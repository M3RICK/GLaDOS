module VM.Interpreter where

import IR.Types
import VM.HelperFunc
import VM.InstructionHandlers

-- The Pâté Carrefour (Main dispatcher)
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
