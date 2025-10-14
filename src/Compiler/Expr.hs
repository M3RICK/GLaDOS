module Compiler.Expr (compileExpr) where

import qualified IR.Types as IR
import AST.AST
import Compiler.Environment
import Security.ExprChecker (getExprType)
import Security.Types (CheckEnv)

-- Compile an expression into IR instructions
compileExpr :: CheckEnv -> FuncTable -> VarTable -> Expr -> [IR.Instruction]
compileExpr checkEnv funcTable varTable expr = case expr of
  BoolLit (Located _ b) -> compileBoolLit b
  NumLit (Located _ n) -> compileNumLit n
  FloatLit (Located _ f) -> compileFloatLit f
  Var (Located _ name) -> compileVar varTable name
  BinOp op (Located _ e1) (Located _ e2) ->
    compileBinOp checkEnv funcTable varTable op e1 e2
  UnOp op (Located _ e) ->
    compileUnOp checkEnv funcTable varTable op e
  Call (Located _ name) args ->
    compileCall checkEnv funcTable varTable name args

compileBoolLit :: Bool -> [IR.Instruction]
compileBoolLit b = [IR.PushBool b]

compileNumLit :: Int -> [IR.Instruction]
compileNumLit n = [IR.PushInt n]

compileFloatLit :: Double -> [IR.Instruction]
compileFloatLit f = [IR.PushFloat f]

compileVar :: VarTable -> String -> [IR.Instruction]
compileVar varTable name = [IR.GetLocal (lookupVar varTable name)]

compileBinOp :: CheckEnv -> FuncTable -> VarTable -> Op -> Expr -> Expr -> [IR.Instruction]
compileBinOp checkEnv funcTable varTable op e1 e2 =
  compileBothOperands checkEnv funcTable varTable e1 e2 ++
  [selectOpInstruction checkEnv op e1]

compileUnOp :: CheckEnv -> FuncTable -> VarTable -> UnOp -> Expr -> [IR.Instruction]
compileUnOp checkEnv funcTable varTable op e =
  compileExpr checkEnv funcTable varTable e ++
  [selectUnOpInstruction checkEnv op e]

compileBothOperands :: CheckEnv -> FuncTable -> VarTable -> Expr -> Expr -> [IR.Instruction]
compileBothOperands checkEnv funcTable varTable e1 e2 =
  compileExpr checkEnv funcTable varTable e1 ++
  compileExpr checkEnv funcTable varTable e2

-- Choose your instruction for an operator based on the operand type
selectOpInstruction :: CheckEnv -> Op -> Expr -> IR.Instruction
selectOpInstruction checkEnv op expr =
  case getExprType checkEnv expr of
    Right TypeFloat -> selectFloatOp op
    Right TypeInt -> selectIntOp op
    Right TypeBool -> selectBoolOp op
    Left err -> error $ "Type error during compilation: " ++ show err
    Right other -> error $ "Unexpected type for operation: " ++ show other

selectIntOp :: Op -> IR.Instruction
selectIntOp Add = IR.AddInt
selectIntOp Sub = IR.SubInt
selectIntOp Mul = IR.MulInt
selectIntOp Div = IR.DivInt
selectIntOp Eq = IR.EqInt
selectIntOp Neq = IR.NeqInt
selectIntOp Lt = IR.LtInt
selectIntOp Gt = IR.GtInt
selectIntOp Le = IR.LeInt
selectIntOp Ge = IR.GeInt
selectIntOp _ = error "Invalid int operation"

selectFloatOp :: Op -> IR.Instruction
selectFloatOp Add = IR.AddFloat
selectFloatOp Sub = IR.SubFloat
selectFloatOp Mul = IR.MulFloat
selectFloatOp Div = IR.DivFloat
selectFloatOp Eq = IR.EqFloat
selectFloatOp Neq = IR.NeqFloat
selectFloatOp Lt = IR.LtFloat
selectFloatOp Gt = IR.GtFloat
selectFloatOp Le = IR.LeFloat
selectFloatOp Ge = IR.GeFloat
selectFloatOp _ = error "Invalid float operation"

selectBoolOp :: Op -> IR.Instruction
selectBoolOp And = IR.AndBool
selectBoolOp Or = IR.OrBool
selectBoolOp _ = error "Invalid bool operation"

-- Choose instruction for unary operator based on operand type
selectUnOpInstruction :: CheckEnv -> UnOp -> Expr -> IR.Instruction
selectUnOpInstruction checkEnv op expr =
  case getExprType checkEnv expr of
    Right TypeFloat -> selectFloatUnOp op
    Right TypeInt -> selectIntUnOp op
    Right TypeBool -> selectBoolUnOp op
    Left err -> error $ "Type error during compilation: " ++ show err
    Right other -> error $ "Unexpected type for unary operation: " ++ show other

selectIntUnOp :: UnOp -> IR.Instruction
selectIntUnOp Neg = IR.NegInt
selectIntUnOp _ = error "Invalid int unary operation"

selectFloatUnOp :: UnOp -> IR.Instruction
selectFloatUnOp Neg = IR.NegFloat
selectFloatUnOp _ = error "Invalid float unary operation"

selectBoolUnOp :: UnOp -> IR.Instruction
selectBoolUnOp Not = IR.NotBool
selectBoolUnOp _ = error "Invalid bool unary operation"

-- Compile function call
compileCall :: CheckEnv -> FuncTable -> VarTable -> String -> [Expr] -> [IR.Instruction]
compileCall checkEnv funcTable varTable name args =
  compileArgumentsAndCall checkEnv funcTable varTable name args

compileArgumentsAndCall :: CheckEnv -> FuncTable -> VarTable -> String -> [Expr] -> [IR.Instruction]
compileArgumentsAndCall checkEnv funcTable varTable name args =
  compileAllArguments checkEnv funcTable varTable args ++
  [createCallInstruction funcTable name]

-- compile tout les fonction args de gauche a droite
compileAllArguments :: CheckEnv -> FuncTable -> VarTable -> [Expr] -> [IR.Instruction]
compileAllArguments checkEnv funcTable varTable args =
  concatMap (compileExpr checkEnv funcTable varTable) args

createCallInstruction :: FuncTable -> String -> IR.Instruction
createCallInstruction funcTable name =
  IR.Call (lookupFunc funcTable name)
