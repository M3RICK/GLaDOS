module Compiler.Core (compileProgram, compileToText, compileToIR) where

import IR.Types
import IR.TextFormat
import AST.AST
import Compiler.Function
import Compiler.Environment
import Error.Types (CompilerError)

-- AST -> IR
compileProgram :: Program -> Either CompilerError IRProgram
compileProgram (Program funcs) =
  case compileFunctions funcs of
    Left err -> Left err
    Right compiledFuncs -> Right $ IRProgram
      { functions = compiledFuncs
      , mainIndex = findMainIndex funcs
      }

-- Badabing badaboom ca compile ca convertit en qqch de lisible
compileToText :: Program -> Either CompilerError String
compileToText program =
  case compileProgram program of
    Left err -> Left err
    Right irProgram -> Right (showProgram irProgram)

-- Compile to IR only (no I/O)
compileToIR :: Program -> Either CompilerError IRProgram
compileToIR = compileProgram
