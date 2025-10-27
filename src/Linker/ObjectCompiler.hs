module Linker.ObjectCompiler
  ( irToObject
  , irToObjectWithNames
  , saveObjectToFile
  , loadObjectFromFile
  ) where

import Data.Binary (encode, decode)
import qualified Data.ByteString.Lazy as BSL
import Control.Exception (try, IOException)
import qualified Data.Set as S

import IR.Types
import Linker.Types
import Linker.BinaryInstances ()

irToObjectWithNames :: IRProgram -> [String] -> ObjectFile
irToObjectWithNames (IRProgram funcs _) allNames =
  ObjectFile relocatableFuncs exportedNames importedNames
  where
    relocatableFuncs = map (toRelocatable allNames) funcs
    exportedNames = map funcName funcs
    importedNames = collectImportsExcludingExports relocatableFuncs exportedNames

irToObject :: IRProgram -> ObjectFile
irToObject prog@(IRProgram funcs _) = irToObjectWithNames prog functionNames
  where
    functionNames = map funcName funcs

toRelocatable :: [String] -> CompiledFunction -> RelocatableFunction
toRelocatable allNames cf = RelocatableFunction
  { rfName = funcName cf
  , rfParams = paramCount cf
  , rfLocals = localVarCount cf
  , rfCode = map (toRelocatableInstr allNames) (code cf)
  }

toRelocatableInstr :: [String] -> Instruction -> RelocatableInstr
toRelocatableInstr allNames (Call idx)
  | isValidIndex = CallRef functionName
  | otherwise = CallRef invalidName
  where
    isValidIndex = idx >= 0 && idx < length allNames
    functionName = allNames !! idx
    invalidName = "__invalid_call_" ++ show idx
toRelocatableInstr _ instr = Fixed instr

collectImportsExcludingExports :: [RelocatableFunction] -> [String] -> [String]
collectImportsExcludingExports funcs exports = S.toList externalReferences
  where
    allReferences = S.fromList $ concatMap extractCallRefs funcs
    exportedSet = S.fromList exports
    externalReferences = S.difference allReferences exportedSet
    extractCallRefs :: RelocatableFunction -> [String]
    extractCallRefs rf = [name | CallRef name <- rfCode rf]

saveObjectToFile :: FilePath -> ObjectFile -> IO (Either String ())
saveObjectToFile path obj = do
  writeResult <- try (BSL.writeFile path encodedObj)
  return $ case writeResult of
    Left err -> Left $ "Failed to write: " ++ show (err :: IOException)
    Right () -> Right ()
  where
    encodedObj = encode obj

loadObjectFromFile :: FilePath -> IO (Either String ObjectFile)
loadObjectFromFile path = do
  readResult <- try (BSL.readFile path)
  return $ case readResult of
    Left err -> Left $ "Failed to read: " ++ show (err :: IOException)
    Right bytes -> Right (decode bytes)
