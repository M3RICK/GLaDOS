module Compiler.Core where

import IR.Types
import IR.TextFormat
import AST.AST
import Compiler.Function
import Compiler.Environment

-- AST -> IR
compileProgram :: Program -> IRProgram
compileProgram (Program funcs) =
  IRProgram
    { functions = compileFunctions funcs
    , mainIndex = findMainIndex funcs
    }

-- Badabing badaboom ca compile ca convertit en qqch de lisible
compileToText :: Program -> String
compileToText program = showProgram (compileProgram program)
