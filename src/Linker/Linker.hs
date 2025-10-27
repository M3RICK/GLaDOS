module Linker.Linker
  ( linkObjects
  , linkObjectFiles
  ) where

import qualified Data.Map as M
import qualified Data.Set as S

import IR.Types
import Linker.Types
import Linker.ObjectCompiler (loadObjectFromFile)

linkObjects :: [ObjectFile] -> Either String IRProgram
linkObjects objs = do
  symTable <- buildSymbolTable objs
  checkUndefinedSymbols objs symTable
  linkedFuncs <- mapM (resolveFunction symTable) allFunctions
  mainIdx <- findMainFunction symTable
  return $ IRProgram linkedFuncs mainIdx
  where
    allFunctions = concatMap objFunctions objs

indexed :: [String] -> [(String, Int)]
indexed items = zipWith (,) items [0..]

buildSymbolTable :: [ObjectFile] -> Either String (M.Map String Int)
buildSymbolTable objs = do
  checkDuplicates funcNames
  return symbolTable
  where
    allFuncs = concatMap objFunctions objs
    funcNames = map rfName allFuncs
    symbolTable = M.fromList (indexed funcNames)

findMainFunction :: M.Map String Int -> Either String Int
findMainFunction symTable =
  case M.lookup "main" symTable of
    Nothing -> Left "No main function found"
    Just idx -> Right idx

checkDuplicates :: [String] -> Either String ()
checkDuplicates names
  | null duplicateNames = Right ()
  | otherwise = Left $ "Duplicate symbols: " ++ show duplicateNames
  where
    duplicateNames = findDuplicates names

findDuplicates :: Ord a => [a] -> [a]
findDuplicates items = M.keys itemsWithMultipleOccurrences
  where
    occurrenceCounts = countOccurrences items
    itemsWithMultipleOccurrences = M.filter (> 1) occurrenceCounts

countOccurrences :: Ord a => [a] -> M.Map a Int
countOccurrences items = M.fromListWith (+) [(item, 1) | item <- items]

checkUndefinedSymbols :: [ObjectFile] -> M.Map String Int -> Either String ()
checkUndefinedSymbols objs symTable
  | S.null missingSymbols = Right ()
  | otherwise = Left $ "Undefined symbols: " ++ show (S.toList missingSymbols)
  where
    allImports = S.fromList $ concatMap objImports objs
    definedSymbols = S.fromList $ M.keys symTable
    missingSymbols = S.difference allImports definedSymbols

resolveFunction :: M.Map String Int -> RelocatableFunction -> Either String CompiledFunction
resolveFunction symTable rf = do
  resolvedCode <- mapM (resolveInstr symTable rf) (rfCode rf)
  return $ CompiledFunction
    { funcName = rfName rf
    , paramCount = rfParams rf
    , localVarCount = rfLocals rf
    , code = resolvedCode
    }

resolveInstr :: M.Map String Int -> RelocatableFunction -> RelocatableInstr -> Either String Instruction
resolveInstr _ _ (Fixed instr) = Right instr
resolveInstr symTable _ (CallRef name) =
  case M.lookup name symTable of
    Nothing -> Left $ "Undefined symbol: " ++ name
    Just idx -> Right (Call idx)

linkObjectFiles :: [FilePath] -> IO (Either String IRProgram)
linkObjectFiles paths = do
  loadResults <- mapM loadObjectFromFile paths
  return $ case sequence loadResults of
    Left err -> Left err
    Right objs -> linkObjects objs
