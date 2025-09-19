{-# LANGUAGE LambdaCase #-}

module Compilator.Compiler.Codegen (compileModule) where

import Compilator.Frontend.AST
import Compilator.Bytecode.Opcode
import qualified Data.Map.Strict as M

type Locals = M.Map Name Int

compileModule :: Program -> Module
compileModule (Program tops) =
  let (funs, mainCode) = foldl step ([], []) tops
      mainFun = Fun "main" 0 (mainCode ++ [HALT])
  in Module 1 (funs ++ [mainFun])
  where
    step (fs, mc) = \case
      TDecl (DFunc n ps body) ->
        let locs = M.fromList (zip ps [0..])
            code = cgExpr locs body ++ [RET]
        in (fs ++ [Fun n (length ps) code], mc)
      TDecl (DLet n e) ->
        (fs, mc ++ cgExpr M.empty e ++ [GSTORE n])
      TStmt (SPrint e) ->
        (fs, mc ++ cgExpr M.empty e ++ [PRINT])

cgExpr :: Locals -> Expr -> [Instr]
cgExpr locs = \case
  ELitInt i    -> [PUSH_INT (fromInteger i)]
  EVar n       ->
    case M.lookup n locs of
      Just ix -> [LOAD (fromIntegral ix)]
      Nothing -> [GLOAD n]
  ECall f args -> concatMap (cgExpr locs) args ++ [CALL f (fromIntegral (length args))]
  EBin Add l r -> cgExpr locs l ++ cgExpr locs r ++ [ADD]
  EBin Mul l r -> cgExpr locs l ++ cgExpr locs r ++ [MUL]
