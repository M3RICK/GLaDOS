module Linker.Types
  ( ObjectFile(..)
  , RelocatableFunction(..)
  , RelocatableInstr(..)
  ) where

import IR.Types (Instruction)

data ObjectFile = ObjectFile
  { objFunctions :: [RelocatableFunction]
  , objExports :: [String] -- What this file provides
  , objImports :: [String] -- What this file needs
  } deriving (Show, Eq)

data RelocatableFunction = RelocatableFunction
  { rfName :: String
  , rfParams :: Int
  , rfLocals :: Int
  , rfCode :: [RelocatableInstr] -- Instructions with symbolic references
  } deriving (Show, Eq)

data RelocatableInstr
  = Fixed Instruction
  | CallRef String
  deriving (Show, Eq)
