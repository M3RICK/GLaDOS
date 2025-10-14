module IR.TextFormat where

import IR.Types
import Data.List (intercalate)

-- Fonction qui va tout rendre visible, c est ca qu il faut appeller
showProgram :: IRProgram -> String
showProgram (IRProgram funcs mainIdx) = unlines $
  [ "// GLaDOS IR - Compiled Program"
  , "// Entry point: function " ++ show mainIdx
  , ""
  ] ++ map showFunction (zip [0..] funcs)

-- Single function
showFunction :: (Int, CompiledFunction) -> String
showFunction (idx, CompiledFunction name params locals instrs) = unlines $
  [ "U0 " ++ name ++ "()  // Function " ++ show idx
  , "{"
  , "  // params: " ++ show params ++ ", locals: " ++ show locals
  ] ++ zipWith formatInstrLine [0..] instrs ++
  [ "}", "" ]

-- Single instruction line with index
formatInstrLine :: Int -> Instruction -> String
formatInstrLine idx instr =
  "  " ++ padLeft 4 (show idx) ++ ": " ++ formatInstruction instr ++ ";"

-- Big intrstruction formatter, ca delegue le boulot
formatInstruction :: Instruction -> String
formatInstruction instr = case instr of
  -- Stack operations
  PushInt n -> formatStackOp "PUSH.I64" (show n)
  PushBool b -> formatStackOp "PUSH.I32" (if b then "1" else "0")
  PushFloat f -> formatStackOp "PUSH.F64" (show f)
  Pop -> "POP"

  -- Variables
  GetLocal n -> formatVarOp "GET" n
  SetLocal n -> formatVarOp "SET" n

  -- Arithmetic (Int)
  AddInt -> formatArithOp "ADD"
  SubInt -> formatArithOp "SUB"
  MulInt -> formatArithOp "MUL"
  DivInt -> formatArithOp "DIV"

  -- Arithmetic (Float)
  AddFloat -> "ADD.F64"
  SubFloat -> "SUB.F64"
  MulFloat -> "MUL.F64"
  DivFloat -> "DIV.F64"

  -- Unary operations
  NegInt -> "NEG.I64"
  NegFloat -> "NEG.F64"
  NotBool -> "NOT.I32"

  -- Comparisons (Int)
  EqInt -> formatCompOp "EQ"
  NeqInt -> formatCompOp "NEQ"
  LtInt -> formatCompOp "LT"
  GtInt -> formatCompOp "GT"
  LeInt -> formatCompOp "LE"
  GeInt -> formatCompOp "GE"

  -- Comparisons (Float)
  EqFloat -> "CMP.EQ.F64"
  NeqFloat -> "CMP.NEQ.F64"
  LtFloat -> "CMP.LT.F64"
  GtFloat -> "CMP.GT.F64"
  LeFloat -> "CMP.LE.F64"
  GeFloat -> "CMP.GE.F64"

  -- Logical
  AndBool -> formatLogicOp "AND"
  OrBool -> formatLogicOp "OR"

  -- Control flow
  Jump n -> formatJumpOp "JMP" n
  JumpIfFalse n -> formatJumpOp "JZ" n  -- Jump if Zero (false)
  Call n -> formatCallOp n
  Return -> "RET"
  Halt -> "HALT"

formatStackOp :: String -> String -> String
formatStackOp op val = op ++ " " ++ val

formatVarOp :: String -> Int -> String
formatVarOp op idx = op ++ ".LOCAL[" ++ show idx ++ "]"

formatArithOp :: String -> String
formatArithOp op = op ++ ".I64"

formatCompOp :: String -> String
formatCompOp op = "CMP." ++ op

formatLogicOp :: String -> String
formatLogicOp op = op ++ ".I32"

formatJumpOp :: String -> Int -> String
formatJumpOp op target = op ++ " @" ++ show target

formatCallOp :: Int -> String
formatCallOp funcIdx = "CALL func" ++ show funcIdx

padLeft :: Int -> String -> String
padLeft width s = replicate (width - length s) ' ' ++ s
