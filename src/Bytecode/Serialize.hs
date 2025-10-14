module Bytecode.Serialize where

import Data.Binary (encode, decode)
-- encode: Takes any Binary type -> ByteString (file-ready bytes)
-- decode: Takes ByteString -> any Binary type
import qualified Data.ByteString.Lazy as BSL
-- ByteString.Lazy: read/write files
import IR.Types
import Bytecode.BcTypes ()

serializeProgram :: IRProgram -> BSL.ByteString
serializeProgram program = encode program

deserializeProgram :: BSL.ByteString -> IRProgram
deserializeProgram bytes = decode bytes


serializeProgramToStdout :: IRProgram -> IO ()
serializeProgramToStdout program =
    BSL.putStr (serializeProgram program)

-- --run mode
loadProgramFromFile :: FilePath -> IO IRProgram
loadProgramFromFile path = do
    bytes <- BSL.readFile path
    return $ deserializeProgram bytes

saveProgramToFile :: FilePath -> IRProgram -> IO ()
saveProgramToFile path program =
    BSL.writeFile path (serializeProgram program)
