module Compiler.Function where

import IR.Types
import AST.AST
import Compiler.Environment
import Compiler.Statement

-- | Compile a single function into IR
compileFunction :: FuncTable -> Function -> CompiledFunction
compileFunction funcTable func =
  CompiledFunction
    { funcName = fName func
    , paramCount = countParameters func
    , localVarCount = countLocalVariables func
    , code = compileFunctionBody funcTable func
    }

countParameters :: Function -> Int
countParameters func = length (fParams func)

countLocalVariables :: Function -> Int
countLocalVariables func = length (collectLocalDecls (fBody func))

-- Function body hop ca devient des instructions
compileFunctionBody :: FuncTable -> Function -> [Instruction]
compileFunctionBody funcTable func =
  compileStatements funcTable variableTable (fBody func)
  where
    variableTable = buildVarTable (fParams func) (fBody func)

-- et la on compile pas une, pas deux mais toutes les fonctions dans le programme
compileFunctions :: [Function] -> [CompiledFunction]
compileFunctions funcs =
  map (compileFunction functionTable) funcs
  where
    functionTable = makeFuncTable funcs
