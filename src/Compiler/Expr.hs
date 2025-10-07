module Compiler.Expr where

import qualified IR.Types as IR
import AST.AST
import Compiler.Environment

-- | Compile an expression into IR instructions
compileExpr :: FuncTable -> VarTable -> Expr -> [IR.Instruction]
compileExpr funcTable varTable expr = case expr of
  BoolLit (Located _ b) -> compileBoolLit b
  NumLit (Located _ n) -> compileNumLit n
  Var (Located _ name) -> compileVar varTable name
  BinOp op (Located _ e1) (Located _ e2) ->
    compileBinOp funcTable varTable op e1 e2
  Call (Located _ name) args ->
    compileCall funcTable varTable name args

compileBoolLit :: Bool -> [IR.Instruction]
compileBoolLit b = [IR.PushBool b]

compileNumLit :: Int -> [IR.Instruction]
compileNumLit n = [IR.PushInt n]

compileVar :: VarTable -> String -> [IR.Instruction]
compileVar varTable name = [IR.GetLocal (lookupVar varTable name)]

compileBinOp :: FuncTable -> VarTable -> Op -> Expr -> Expr -> [IR.Instruction]
compileBinOp funcTable varTable op e1 e2 =
  compileBothOperands funcTable varTable e1 e2 ++
  [selectOpInstruction op]

compileBothOperands :: FuncTable -> VarTable -> Expr -> Expr -> [IR.Instruction]
compileBothOperands funcTable varTable e1 e2 =
  compileExpr funcTable varTable e1 ++
  compileExpr funcTable varTable e2

-- Choose your instruction for an operator
selectOpInstruction :: Op -> IR.Instruction
selectOpInstruction Add = IR.AddInt
selectOpInstruction Sub = IR.SubInt
selectOpInstruction Mul = IR.MulInt
selectOpInstruction Div = IR.DivInt
selectOpInstruction Eq = IR.EqInt
selectOpInstruction Neq = IR.NeqInt
selectOpInstruction Lt = IR.LtInt
selectOpInstruction Gt = IR.GtInt
selectOpInstruction Le = IR.LeInt
selectOpInstruction Ge = IR.GeInt
selectOpInstruction And = IR.AndBool
selectOpInstruction Or = IR.OrBool

-- Compile function call
compileCall :: FuncTable -> VarTable -> String -> [Expr] -> [IR.Instruction]
compileCall funcTable varTable name args =
  compileArgumentsAndCall funcTable varTable name args

compileArgumentsAndCall :: FuncTable -> VarTable -> String -> [Expr] -> [IR.Instruction]
compileArgumentsAndCall funcTable varTable name args =
  compileAllArguments funcTable varTable args ++
  [createCallInstruction funcTable name]

-- compile tout les fonction args de gauche a droite
compileAllArguments :: FuncTable -> VarTable -> [Expr] -> [IR.Instruction]
compileAllArguments funcTable varTable args =
  concatMap (compileExpr funcTable varTable) args

createCallInstruction :: FuncTable -> String -> IR.Instruction
createCallInstruction funcTable name =
  IR.Call (lookupFunc funcTable name)
