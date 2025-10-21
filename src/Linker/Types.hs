module Linker.Types
  ( ObjectFile(..)
  , RelocatableFunction(..)
  , RelocatableInstr(..)
  ) where

import IR.Types (Instruction)

-- Object file: what gets saved to disk
data ObjectFile = ObjectFile
  { objFunctions :: [RelocatableFunction]
  , objExports :: [String]        -- Ce que ca apporte
  , objImports :: [String]        -- En a besoin
  } deriving (Show, Eq)

data RelocatableFunction = RelocatableFunction
  { rfName :: String
  , rfParams :: Int
  , rfLocals :: Int
  , rfCode :: [RelocatableInstr]
  } deriving (Show, Eq)

data RelocatableInstr
  = Fixed Instruction      -- Normal instruction
  | CallRef String        -- Call by name (needs linking)
  deriving (Show, Eq)
