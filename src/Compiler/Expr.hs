module Compiler.Expr (compileExpr) where

import qualified IR.Types as IR
import AST.AST
import Compiler.Environment
import Security.ExprChecker (getExprType)
import Security.Types (CheckEnv)
import Error.Types (CompilerError(..))
import Text.Megaparsec.Pos (SourcePos)

-- Compile an expression into IR instructions
-- Returns Either CompilerError if compilation fails (shouldn't happen after type checking)
compileExpr :: CheckEnv -> FuncTable -> VarTable -> Expr -> Either CompilerError [IR.Instruction]
compileExpr checkEnv funcTable varTable expr = case expr of
  BoolLit (Located _ b) -> Right (compileBoolLit b)
  NumLit (Located _ n) -> Right (compileNumLit n)
  FloatLit (Located _ f) -> Right (compileFloatLit f)
  Var (Located _ name) -> Right (compileVar varTable name)
  BinOp op (Located srcPos e1) (Located _ e2) ->
    compileBinOp checkEnv funcTable varTable op srcPos e1 e2
  UnOp op (Located srcPos e) ->
    compileUnOp checkEnv funcTable varTable op srcPos e
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

compileBinOp :: CheckEnv -> FuncTable -> VarTable -> Op -> SourcePos -> Expr -> Expr -> Either CompilerError [IR.Instruction]
compileBinOp checkEnv funcTable varTable op srcPos e1 e2 =
  case compileBothOperands checkEnv funcTable varTable e1 e2 of
    Left err -> Left err
    Right operands -> case selectOpInstruction checkEnv op srcPos e1 of
      Left err -> Left err
      Right opInstr -> Right (operands ++ [opInstr])

compileUnOp :: CheckEnv -> FuncTable -> VarTable -> UnOp -> SourcePos -> Expr -> Either CompilerError [IR.Instruction]
compileUnOp checkEnv funcTable varTable op srcPos e =
  case compileExpr checkEnv funcTable varTable e of
    Left err -> Left err
    Right exprCode -> case selectUnOpInstruction checkEnv op srcPos e of
      Left err -> Left err
      Right opInstr -> Right (exprCode ++ [opInstr])

compileBothOperands :: CheckEnv -> FuncTable -> VarTable -> Expr -> Expr -> Either CompilerError [IR.Instruction]
compileBothOperands checkEnv funcTable varTable e1 e2 =
  case compileExpr checkEnv funcTable varTable e1 of
    Left err -> Left err
    Right code1 -> case compileExpr checkEnv funcTable varTable e2 of
      Left err -> Left err
      Right code2 -> Right (code1 ++ code2)

-- Choose your instruction for an operator based on the operand type
selectOpInstruction :: CheckEnv -> Op -> SourcePos -> Expr -> Either CompilerError IR.Instruction
selectOpInstruction checkEnv op srcPos expr =
  case getExprType checkEnv expr of
    Right TypeFloat -> selectFloatOp op srcPos
    Right TypeInt -> selectIntOp op srcPos
    Right TypeBool -> selectBoolOp op srcPos
    Left err -> Left (TypeErrors [err])
    Right other -> Left (CompileError ("Unexpected type for operation: " ++ show other) srcPos)

selectIntOp :: Op -> SourcePos -> Either CompilerError IR.Instruction
selectIntOp Add _ = Right IR.AddInt
selectIntOp Sub _ = Right IR.SubInt
selectIntOp Mul _ = Right IR.MulInt
selectIntOp Div _ = Right IR.DivInt
selectIntOp Eq _ = Right IR.EqInt
selectIntOp Neq _ = Right IR.NeqInt
selectIntOp Lt _ = Right IR.LtInt
selectIntOp Gt _ = Right IR.GtInt
selectIntOp Le _ = Right IR.LeInt
selectIntOp Ge _ = Right IR.GeInt
selectIntOp op srcPos = Left (CompileError ("Invalid int operation: " ++ show op) srcPos)

selectFloatOp :: Op -> SourcePos -> Either CompilerError IR.Instruction
selectFloatOp Add _ = Right IR.AddFloat
selectFloatOp Sub _ = Right IR.SubFloat
selectFloatOp Mul _ = Right IR.MulFloat
selectFloatOp Div _ = Right IR.DivFloat
selectFloatOp Eq _ = Right IR.EqFloat
selectFloatOp Neq _ = Right IR.NeqFloat
selectFloatOp Lt _ = Right IR.LtFloat
selectFloatOp Gt _ = Right IR.GtFloat
selectFloatOp Le _ = Right IR.LeFloat
selectFloatOp Ge _ = Right IR.GeFloat
selectFloatOp op srcPos = Left (CompileError ("Invalid float operation: " ++ show op) srcPos)

selectBoolOp :: Op -> SourcePos -> Either CompilerError IR.Instruction
selectBoolOp And _ = Right IR.AndBool
selectBoolOp Or _ = Right IR.OrBool
selectBoolOp op srcPos = Left (CompileError ("Invalid bool operation: " ++ show op) srcPos)

-- Choose instruction for unary operator based on operand type
selectUnOpInstruction :: CheckEnv -> UnOp -> SourcePos -> Expr -> Either CompilerError IR.Instruction
selectUnOpInstruction checkEnv op srcPos expr =
  case getExprType checkEnv expr of
    Right TypeFloat -> selectFloatUnOp op srcPos
    Right TypeInt -> selectIntUnOp op srcPos
    Right TypeBool -> selectBoolUnOp op srcPos
    Left err -> Left (TypeErrors [err])
    Right other -> Left (CompileError ("Unexpected type for unary operation: " ++ show other) srcPos)

selectIntUnOp :: UnOp -> SourcePos -> Either CompilerError IR.Instruction
selectIntUnOp Neg _ = Right IR.NegInt
selectIntUnOp op srcPos = Left (CompileError ("Invalid int unary operation: " ++ show op) srcPos)

selectFloatUnOp :: UnOp -> SourcePos -> Either CompilerError IR.Instruction
selectFloatUnOp Neg _ = Right IR.NegFloat
selectFloatUnOp op srcPos = Left (CompileError ("Invalid float unary operation: " ++ show op) srcPos)

selectBoolUnOp :: UnOp -> SourcePos -> Either CompilerError IR.Instruction
selectBoolUnOp Not _ = Right IR.NotBool
selectBoolUnOp op srcPos = Left (CompileError ("Invalid bool unary operation: " ++ show op) srcPos)

-- Compile function call
compileCall :: CheckEnv -> FuncTable -> VarTable -> String -> [Expr] -> Either CompilerError [IR.Instruction]
compileCall checkEnv funcTable varTable name args =
  compileArgumentsAndCall checkEnv funcTable varTable name args

compileArgumentsAndCall :: CheckEnv -> FuncTable -> VarTable -> String -> [Expr] -> Either CompilerError [IR.Instruction]
compileArgumentsAndCall checkEnv funcTable varTable name args =
  case compileAllArguments checkEnv funcTable varTable args of
    Left err -> Left err
    Right argCode -> Right (argCode ++ [createCallInstruction funcTable name])

-- compile tout les fonction args de gauche a droite
compileAllArguments :: CheckEnv -> FuncTable -> VarTable -> [Expr] -> Either CompilerError [IR.Instruction]
compileAllArguments checkEnv funcTable varTable args =
  case mapM (compileExpr checkEnv funcTable varTable) args of
    Left err -> Left err
    Right argCodes -> Right (concat argCodes)

createCallInstruction :: FuncTable -> String -> IR.Instruction
createCallInstruction funcTable name =
  IR.Call (lookupFunc funcTable name)
