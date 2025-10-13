module Bytecode.Serialize where

import Data.Binary (encode, decode)
-- encode: Takes any Binary type -> ByteString (file-ready bytes)
-- decode: Takes ByteString -> any Binary type
import qualified Data.ByteString.Lazy as BSL
-- ByteString.Lazy: Efficient representation of byte sequences
-- We'll use this to read/write files
import IR.Types

-- Serialization helper functions will go here
