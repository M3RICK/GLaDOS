module Compiler.Core (compileProgram, compileToText, compileToIR) where

import qualified Data.Map as M
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import IR.Types
import IR.TextFormat
import AST.AST
import AST.Helpers (extractFunctions)
import Compiler.Function
import Compiler.Environment
import Error.Types (CompilerError)

-- AST -> IR
compileProgram :: Program -> Either CompilerError IRProgram
compileProgram prog@(Program topLevels) =
  let funcs = extractFunctions topLevels
  in case compileFunctionsWithPrototypes prog funcs of
    Left err -> Left err
    Right compiledFuncs -> Right $ IRProgram
      { functions = compiledFuncs
      , mainIndex = findMainIndexFromFuncs funcs
      }

compileFunctionsWithPrototypes :: Program -> [Function] -> Either CompilerError [CompiledFunction]
compileFunctionsWithPrototypes (Program topLevels) funcs =
  let allNames = extractAllFunctionNames topLevels
      functionTable = makeFuncTableWithExternals allNames funcs
  in compileFunctionsWithTable functionTable funcs

extractAllFunctionNames :: [TopLevel] -> [String]
extractAllFunctionNames = concatMap getName
  where
    getName (FuncDef f) = [fName f]
    getName (FuncProto fd) = [fdName fd]

-- Helper: pair each string with its index
indexed :: [String] -> [(String, Int)]
indexed items = zipWith (,) items [0..]

makeFuncTableWithExternals :: [String] -> [Function] -> FuncTable
makeFuncTableWithExternals allNames funcs =
  let defNames = map fName funcs
      allFunctionNames = defNames ++ filter (`notElem` defNames) allNames
  in M.fromList (indexed allFunctionNames)

findMainIndexFromFuncs :: [Function] -> Int
findMainIndexFromFuncs funcs =
  let functionNames = map fName funcs
  in fromMaybe 0 (elemIndex "main" functionNames)


-- Badabing badaboom ca compile ca convertit en qqch de lisible
compileToText :: Program -> Either CompilerError String
compileToText program =
  case compileProgram program of
    Left err -> Left err
    Right irProgram -> Right (showProgram irProgram)

-- Compile to IR only (no I/O)
compileToIR :: Program -> Either CompilerError IRProgram
compileToIR = compileProgram
