module Bytecode.Serialize (
    serializeProgram
  , deserializeProgram
  , serializeProgramToStdout
  , loadProgramFromFile
  , saveProgramToFile
) where

import Data.Binary (encode, decode)
-- encode: Takes any Binary type -> ByteString (file-ready bytes)
-- decode: Takes ByteString -> any Binary type
import qualified Data.ByteString.Lazy as BSL
-- ByteString.Lazy: read/write files
import IR.Types
import Bytecode.BcTypes ()
import Control.Exception (try, IOException)

serializeProgram :: IRProgram -> BSL.ByteString
serializeProgram program = encode program

deserializeProgram :: BSL.ByteString -> Either String IRProgram
deserializeProgram bytes = Right (decode bytes)

serializeProgramToStdout :: IRProgram -> IO ()
serializeProgramToStdout program =
    BSL.putStr (serializeProgram program)

-- --run mode
loadProgramFromFile :: FilePath -> IO (Either String IRProgram)
loadProgramFromFile path = do
    result <- try (BSL.readFile path)
    case result of
        Left err -> return $ Left $ "Failed to read file '" ++ path ++ "': " ++ show (err :: IOException)
        Right bytes -> return $ deserializeProgram bytes

saveProgramToFile :: FilePath -> IRProgram -> IO (Either String ())
saveProgramToFile path program = do
    result <- try (BSL.writeFile path (serializeProgram program))
    case result of
        Left err -> return $ Left $ "Failed to write file '" ++ path ++ "': " ++ show (err :: IOException)
        Right () -> return $ Right ()
