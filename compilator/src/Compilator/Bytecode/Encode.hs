module Compilator.Bytecode.Encode (writeGlbc) where

import Compilator.Bytecode.Opcode
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Binary.Put as Put
import Data.Binary.Put (Put)
import Data.Int
import Data.Word

writeGlbc :: FilePath -> Module -> IO ()
writeGlbc fp m = BL.writeFile fp (Put.runPut (putModule m))

putModule :: Module -> Put
putModule (Module ver funs) = do
  Put.putByteString (BS.pack "GLBC")
  Put.putWord16be ver
  Put.putWord16be (fromIntegral (length funs))
  mapM_ putFun funs

putFun :: Fun -> Put
putFun (Fun name arity code) = do
  putStr16 name
  Put.putWord8 (fromIntegral arity)
  let codeBytes = Put.runPut (mapM_ putInstr code)
  Put.putWord32be (fromIntegral (BL.length codeBytes))
  Put.putLazyByteString codeBytes

putInstr :: Instr -> Put
putInstr i = do
  Put.putWord8 (opcodeOf i)
  case i of
    PUSH_INT n -> Put.putInt64be n
    LOAD ix    -> Put.putWord16be ix
    GLOAD s    -> putStr16 s
    GSTORE s   -> putStr16 s
    CALL s a   -> putStr16 s >> Put.putWord8 a
    _          -> pure ()

opcodeOf :: Instr -> Word8
opcodeOf ins = case ins of
  PUSH_INT{} -> 0x01
  LOAD{}     -> 0x04
  GLOAD{}    -> 0x06
  GSTORE{}   -> 0x07
  ADD        -> 0x10
  MUL        -> 0x12
  CALL{}     -> 0x40
  RET        -> 0x41
  PRINT      -> 0x50
  HALT       -> 0x51

putStr16 :: String -> Put
putStr16 s = do
  Put.putWord16be (fromIntegral (length s))
  mapM_ (Put.putWord8 . fromIntegral . fromEnum) s
