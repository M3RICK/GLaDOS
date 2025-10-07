module VM.Interpreter where

import IR.Types
import VM.HelperFunc

-- Execute a single instruction and update the VM state
executeInstruction :: Instruction -> VMState -> VMResult VMState
executeInstruction instr state = case instr of
    PushInt n -> Right (push (VInt n) state) -- Stack interactions
    PushBool b -> Right (push (VBool b) state)
    Pop -> case pop state of
        Right (_, newState) -> Right newState
        Left err -> Left err
    GetLocal idx -> case getLocal idx state of -- Local var edditting
        Right val -> Right (push val state)
        Left err -> Left err
    SetLocal idx -> case pop state of
        Right (val, newState) -> setLocal idx val newState
        Left err -> Left err
    AddInt -> binaryIntOp (+) state -- Op with result push
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
    EqInt -> compareInts (==) state -- Comparison operations: pop 2 ints, compare, push bool
    NeqInt -> compareInts (/=) state
    LtInt -> compareInts (<) state
    GtInt -> compareInts (>) state
    LeInt -> compareInts (<=) state
    GeInt -> compareInts (>=) state
    AndBool -> binaryBoolOp (&&) state -- Logical operations: pop 2 bools, compute, push bool
    OrBool -> binaryBoolOp (||) state
    Jump addr -> Right (state {pc = addr}) -- Control flow operations
    JumpIfFalse addr -> case popBool state of
        Left err -> Left err
        Right (condition, newState) ->
            if condition
            then Right newState -- condition is true, don't jump (continue to next instruction)
            else Right (newState {pc = addr}) -- condition is false, jump to addr
    Call _funcIdx -> Left "Call instruction not yet implemented" -- Will implement with main execute loop
    Return -> Left "Return instruction not yet implemented" -- Will implement with main execute loop
    Halt -> Right state -- Halt doesn't change state, just signals to stop execution
