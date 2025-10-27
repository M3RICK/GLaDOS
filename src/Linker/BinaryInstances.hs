{-# OPTIONS_GHC -Wno-orphans #-}
module Linker.BinaryInstances () where

import Data.Binary
import Linker.Types
import Bytecode.BcTypes ()

instance Binary RelocatableInstr where
  put (Fixed instr) = do
    putWord8 0 -- Tag for Fixed
    put instr
  put (CallRef name) = do
    putWord8 1 -- Tag for CallRef
    put name
  get = do
    tag <- getWord8
    case tag of
      0 -> Fixed <$> get
      1 -> CallRef <$> get
      _ -> fail "Invalid RelocatableInstr tag"

instance Binary RelocatableFunction where
  put (RelocatableFunction n p l c) = do
    put n -- Function name
    put p -- Parameter count
    put l -- Local variable count
    put c -- Instruction list
  get = RelocatableFunction <$> get <*> get <*> get <*> get

instance Binary ObjectFile where
  put (ObjectFile funcs exports imports) = do
    putWord8 0x4F  -- 'O'
    putWord8 0x42  -- 'B'
    putWord8 0x4A  -- 'J'
    put funcs -- All functions in this object file
    put exports -- Names of functions we provide
    put imports -- Names of functions we need
  get = do
    magicBytes <- sequence [getWord8, getWord8, getWord8]
    if magicBytes /= expectedMagic
      then fail "Invalid object file magic"
      else ObjectFile <$> get <*> get <*> get
    where
      expectedMagic = [0x4F, 0x42, 0x4A]
