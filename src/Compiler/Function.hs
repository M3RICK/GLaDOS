module Compiler.Function (compileFunction, compileFunctions) where

import IR.Types
import AST.AST
import Compiler.Environment
import Compiler.Statement
import qualified Security.Environment as SE
import Security.Types (CheckEnv)
import Error.Types (CompilerError)

-- single function tiouf dans l'IR
compileFunction :: CheckEnv -> FuncTable -> Function -> Either CompilerError CompiledFunction
compileFunction checkEnv funcTable func =
  case compileFunctionBody checkEnv funcTable func of
    Left err -> Left err
    Right bodyCode -> Right $ CompiledFunction
      { funcName = fName func
      , paramCount = countParameters func
      , localVarCount = countLocalVariables func
      , code = bodyCode
      }

countParameters :: Function -> Int
countParameters func = length (fParams func)

countLocalVariables :: Function -> Int
countLocalVariables func = length (collectLocalDecls (fBody func))

-- Function body hop ca devient des instructions
compileFunctionBody :: CheckEnv -> FuncTable -> Function -> Either CompilerError [Instruction]
compileFunctionBody checkEnv funcTable func =
  compileStatements completeEnv funcTable variableTable (fBody func)
  where
    variableTable = buildVarTable (fParams func) (fBody func)
    localDecls = collectLocalDecls (fBody func)
    completeEnv = foldr addLocalToEnv checkEnv localDecls

addLocalToEnv :: (String, Type) -> CheckEnv -> CheckEnv
addLocalToEnv (name, typ) env =
  let envWithVar = SE.addVar name typ env
  in SE.markInitialized name envWithVar

-- et la on compile pas une, pas deux mais toutes les fonctions dans le programme
compileFunctions :: [Function] -> Either CompilerError [CompiledFunction]
compileFunctions funcs =
  case mapM compileWithEnv funcs of
    Left err -> Left err
    Right compiledFuncs -> Right compiledFuncs
  where
    functionTable = makeFuncTable funcs
    funcEnv = SE.collectFunctionSignatures funcs
    compileWithEnv func = compileFunction (SE.makeFunctionEnv funcEnv func) functionTable func
