module Compilator.Bytecode.Opcode where

import Data.Int
import Data.Word

data Instr
  = PUSH_INT  Int64
  | LOAD      Word16
  | GLOAD     String
  | GSTORE    String
  | ADD | MUL
  | CALL      String Word8
  | RET
  | PRINT
  | HALT
  deriving (Show, Eq)

data Fun = Fun
  { funName  :: String
  , funArity :: Int
  , funCode  :: [Instr]
  } deriving (Show, Eq)

data Module = Module
  { modVersion :: Word16
  , modFuns    :: [Fun]
  } deriving (Show, Eq)
