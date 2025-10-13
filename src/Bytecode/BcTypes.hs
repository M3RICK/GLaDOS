module Bytecode.BcTypes where

import Data.Binary (Binary, get, put, getWord8, putWord8)
-- Binary: The typeclass we'll implement (like Show or Eq)
-- get: Function to deserialize (read bytes -> data)
-- put: Function to serialize (write data -> bytes)
-- getWord8: Read a single byte (0-255)
-- putWord8: Write a single byte (0-255)

import IR.Types

-- Binary instances will go here
