module Compiler.Statement where

import qualified IR.Types as IR
import AST.AST
import Compiler.Environment
import Compiler.Expr

-- Statement into IR instructions
compileStatement :: FuncTable -> VarTable -> Statement -> [IR.Instruction]
compileStatement funcTable varTable stmt = case stmt of
  Decl _ name maybeExpr -> compileDecl funcTable varTable name maybeExpr
  Assign name expr -> compileAssign funcTable varTable name expr
  If cond thenBody elseBody -> compileIf funcTable varTable cond thenBody elseBody
  While cond body -> compileWhile funcTable varTable cond body
  Return expr -> compileReturn funcTable varTable expr
  ExprStmt expr -> compileExprStmt funcTable varTable expr

-- Variable declaration
compileDecl :: FuncTable -> VarTable -> String -> Maybe Expr -> [IR.Instruction]
compileDecl funcTable varTable name maybeExpr =
  case maybeExpr of
    Nothing -> []
    Just expr -> compileDeclWithInit funcTable varTable name expr

-- Declaration with initialization
compileDeclWithInit :: FuncTable -> VarTable -> String -> Expr -> [IR.Instruction]
compileDeclWithInit funcTable varTable name expr =
  compileExpr funcTable varTable expr ++
  [IR.SetLocal (lookupVar varTable name)]

-- Compile assignment statement
compileAssign :: FuncTable -> VarTable -> String -> Expr -> [IR.Instruction]
compileAssign funcTable varTable name expr =
  compileExpr funcTable varTable expr ++
  [IR.SetLocal (lookupVar varTable name)]

-- | Compile if statement
compileIf :: FuncTable -> VarTable -> Expr -> [Statement] -> Maybe [Statement] -> [IR.Instruction]
compileIf funcTable varTable cond thenBody Nothing =
  compileSimpleIf funcTable varTable cond thenBody
compileIf funcTable varTable cond thenBody (Just elseBody) =
  compileIfElse funcTable varTable cond thenBody elseBody

-- if (cond) { then }
compileSimpleIf :: FuncTable -> VarTable -> Expr -> [Statement] -> [IR.Instruction]
compileSimpleIf funcTable varTable cond thenBody =
  conditionCode ++
  [IR.JumpIfFalse skipDistance] ++
  thenCode
  where
    conditionCode = compileExpr funcTable varTable cond
    thenCode = compileStatements funcTable varTable thenBody
    skipDistance = length thenCode + 1

-- if (cond) { then } else { else }
compileIfElse :: FuncTable -> VarTable -> Expr -> [Statement] -> [Statement] -> [IR.Instruction]
compileIfElse funcTable varTable cond thenBody elseBody =
  conditionCode ++
  [IR.JumpIfFalse jumpToElse] ++
  thenCode ++
  [IR.Jump jumpToEnd] ++
  elseCode
  where
    conditionCode = compileExpr funcTable varTable cond
    thenCode = compileStatements funcTable varTable thenBody
    elseCode = compileStatements funcTable varTable elseBody
    jumpToElse = calculateJumpToElse thenCode
    jumpToEnd = calculateJumpToEnd elseCode

-- | Calculate jump distance to skip then branch and reach else branch
calculateJumpToElse :: [IR.Instruction] -> Int
calculateJumpToElse thenCode = length thenCode + 2  -- on saute le then et en plus de ca on unconditionnal jump

-- | Calculate jump distance to skip else branch and reach end
calculateJumpToEnd :: [IR.Instruction] -> Int
calculateJumpToEnd elseCode = length elseCode + 1   -- rdv apres le else

-- while (cond) { body }
compileWhile :: FuncTable -> VarTable -> Expr -> [Statement] -> [IR.Instruction]
compileWhile funcTable varTable cond body =
  conditionCode ++
  [IR.JumpIfFalse jumpToEnd] ++
  bodyCode ++
  [IR.Jump jumpBackToStart]
  where
    conditionCode = compileExpr funcTable varTable cond
    bodyCode = compileStatements funcTable varTable body
    jumpToEnd = calculateJumpToLoopEnd bodyCode
    jumpBackToStart = calculateJumpBackToLoopStart conditionCode bodyCode

calculateJumpToLoopEnd :: [IR.Instruction] -> Int
calculateJumpToLoopEnd bodyCode = length bodyCode + 2

-- | Calcule combien d etapes il doit revenir en arriere
calculateJumpBackToLoopStart :: [IR.Instruction] -> [IR.Instruction] -> Int
calculateJumpBackToLoopStart conditionCode bodyCode =
  negate totalLoopSize  -- Retourne a la condition
  where
    totalLoopSize = length conditionCode + 1 + length bodyCode + 1

-- return statement
compileReturn :: FuncTable -> VarTable -> Expr -> [IR.Instruction]
compileReturn funcTable varTable expr =
  compileExprAndReturn funcTable varTable expr

compileExprAndReturn :: FuncTable -> VarTable -> Expr -> [IR.Instruction]
compileExprAndReturn funcTable varTable expr =
  compileExpr funcTable varTable expr ++ [IR.Return]

-- expression statement (result discarded)
compileExprStmt :: FuncTable -> VarTable -> Expr -> [IR.Instruction]
compileExprStmt funcTable varTable expr =
  compileExprAndDiscard funcTable varTable expr

-- | Compile expression and discard result (for side effects only)
compileExprAndDiscard :: FuncTable -> VarTable -> Expr -> [IR.Instruction]
compileExprAndDiscard funcTable varTable expr =
  compileExpr funcTable varTable expr ++ [IR.Pop]

-- Compile multiple statements
compileStatements :: FuncTable -> VarTable -> [Statement] -> [IR.Instruction]
compileStatements funcTable varTable stmts =
  concatMap (compileStatement funcTable varTable) stmts
