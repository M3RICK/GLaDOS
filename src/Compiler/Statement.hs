module Compiler.Statement where

import qualified IR.Types as IR
import AST.AST
import Compiler.Environment
import Compiler.Expr
import Security.Types (CheckEnv)

-- Statement into IR instructions
compileStatement :: CheckEnv -> FuncTable -> VarTable -> Statement -> [IR.Instruction]
compileStatement checkEnv funcTable varTable stmt = case stmt of
  Decl _ name maybeExpr -> compileDecl checkEnv funcTable varTable name maybeExpr
  Assign name expr -> compileAssign checkEnv funcTable varTable name expr
  If cond thenBody elseBody -> compileIf checkEnv funcTable varTable cond thenBody elseBody
  While cond body -> compileWhile checkEnv funcTable varTable cond body
  For init cond update body -> compileFor checkEnv funcTable varTable init cond update body
  Return expr -> compileReturn checkEnv funcTable varTable expr
  ExprStmt expr -> compileExprStmt checkEnv funcTable varTable expr

-- Variable declaration
compileDecl :: CheckEnv -> FuncTable -> VarTable -> String -> Maybe Expr -> [IR.Instruction]
compileDecl checkEnv funcTable varTable name maybeExpr =
  case maybeExpr of
    Nothing -> []
    Just expr -> compileDeclWithInit checkEnv funcTable varTable name expr

-- Declaration with initialization
compileDeclWithInit :: CheckEnv -> FuncTable -> VarTable -> String -> Expr -> [IR.Instruction]
compileDeclWithInit checkEnv funcTable varTable name expr =
  compileExpr checkEnv funcTable varTable expr ++
  [IR.SetLocal (lookupVar varTable name)]

-- Compile assignment statement
compileAssign :: CheckEnv -> FuncTable -> VarTable -> String -> Expr -> [IR.Instruction]
compileAssign checkEnv funcTable varTable name expr =
  compileExpr checkEnv funcTable varTable expr ++
  [IR.SetLocal (lookupVar varTable name)]

-- | Compile if statement
compileIf :: CheckEnv -> FuncTable -> VarTable -> Expr -> [Statement] -> Maybe [Statement] -> [IR.Instruction]
compileIf checkEnv funcTable varTable cond thenBody Nothing =
  compileSimpleIf checkEnv funcTable varTable cond thenBody
compileIf checkEnv funcTable varTable cond thenBody (Just elseBody) =
  compileIfElse checkEnv funcTable varTable cond thenBody elseBody

-- if (cond) { then }
compileSimpleIf :: CheckEnv -> FuncTable -> VarTable -> Expr -> [Statement] -> [IR.Instruction]
compileSimpleIf checkEnv funcTable varTable cond thenBody =
  conditionCode ++
  [IR.JumpIfFalse skipDistance] ++
  thenCode
  where
    conditionCode = compileExpr checkEnv funcTable varTable cond
    thenCode = compileStatements checkEnv funcTable varTable thenBody
    skipDistance = length thenCode + 1

-- if (cond) { then } else { else }
compileIfElse :: CheckEnv -> FuncTable -> VarTable -> Expr -> [Statement] -> [Statement] -> [IR.Instruction]
compileIfElse checkEnv funcTable varTable cond thenBody elseBody =
  conditionCode ++
  [IR.JumpIfFalse jumpToElse] ++
  thenCode ++
  [IR.Jump jumpToEnd] ++
  elseCode
  where
    conditionCode = compileExpr checkEnv funcTable varTable cond
    thenCode = compileStatements checkEnv funcTable varTable thenBody
    elseCode = compileStatements checkEnv funcTable varTable elseBody
    jumpToElse = calculateJumpToElse thenCode
    jumpToEnd = calculateJumpToEnd elseCode

-- | Calculate jump distance to skip then branch and reach else branch
calculateJumpToElse :: [IR.Instruction] -> Int
calculateJumpToElse thenCode = length thenCode + 2  -- on saute le then et en plus de ca on unconditionnal jump

-- | Calculate jump distance to skip else branch and reach end
calculateJumpToEnd :: [IR.Instruction] -> Int
calculateJumpToEnd elseCode = length elseCode + 1   -- rdv apres le else

-- while (cond) { body }
compileWhile :: CheckEnv -> FuncTable -> VarTable -> Expr -> [Statement] -> [IR.Instruction]
compileWhile checkEnv funcTable varTable cond body =
  conditionCode ++
  [IR.JumpIfFalse jumpToEnd] ++
  bodyCode ++
  [IR.Jump jumpBackToStart]
  where
    conditionCode = compileExpr checkEnv funcTable varTable cond
    bodyCode = compileStatements checkEnv funcTable varTable body
    jumpToEnd = calculateJumpToLoopEnd bodyCode
    jumpBackToStart = calculateJumpBackToLoopStart conditionCode bodyCode

calculateJumpToLoopEnd :: [IR.Instruction] -> Int
calculateJumpToLoopEnd bodyCode = length bodyCode + 2

-- Calcule combien d etapes il doit revenir en arriere
calculateJumpBackToLoopStart :: [IR.Instruction] -> [IR.Instruction] -> Int
calculateJumpBackToLoopStart conditionCode bodyCode =
  negate totalLoopSize  -- Retourne a la condition
  where
    totalLoopSize = length conditionCode + 1 + length bodyCode

-- return statement
compileReturn :: CheckEnv -> FuncTable -> VarTable -> Expr -> [IR.Instruction]
compileReturn checkEnv funcTable varTable expr =
  compileExprAndReturn checkEnv funcTable varTable expr

compileExprAndReturn :: CheckEnv -> FuncTable -> VarTable -> Expr -> [IR.Instruction]
compileExprAndReturn checkEnv funcTable varTable expr =
  compileExpr checkEnv funcTable varTable expr ++ [IR.Return]

-- expression statement (result discarded)
compileExprStmt :: CheckEnv -> FuncTable -> VarTable -> Expr -> [IR.Instruction]
compileExprStmt checkEnv funcTable varTable expr =
  compileExprAndDiscard checkEnv funcTable varTable expr

-- | Compile expression and discard result (for side effects only)
compileExprAndDiscard :: CheckEnv -> FuncTable -> VarTable -> Expr -> [IR.Instruction]
compileExprAndDiscard checkEnv funcTable varTable expr =
  compileExpr checkEnv funcTable varTable expr ++ [IR.Pop]

-- Compile multiple statements
compileStatements :: CheckEnv -> FuncTable -> VarTable -> [Statement] -> [IR.Instruction]
compileStatements checkEnv funcTable varTable stmts =
  concatMap (compileStatement checkEnv funcTable varTable) stmts

-- for (init; cond; update) { body }
compileFor :: CheckEnv -> FuncTable -> VarTable -> Maybe Statement -> Maybe Expr -> Maybe Statement -> [Statement] -> [IR.Instruction]
compileFor checkEnv funcTable varTable initStmt condExpr updateStmt body =
  initCode ++
  conditionCode ++
  [IR.JumpIfFalse jumpToEnd] ++
  bodyCode ++
  updateCode ++
  [IR.Jump jumpBackToStart]
  where
    initCode = compileForInit checkEnv funcTable varTable initStmt
    conditionCode = compileForCondition checkEnv funcTable varTable condExpr
    bodyCode = compileStatements checkEnv funcTable varTable body
    updateCode = compileForUpdate checkEnv funcTable varTable updateStmt
    jumpToEnd = calculateForLoopEnd bodyCode updateCode
    jumpBackToStart = calculateForLoopStart conditionCode bodyCode updateCode

-- for loop init (runs once before loop)
compileForInit :: CheckEnv -> FuncTable -> VarTable -> Maybe Statement -> [IR.Instruction]
compileForInit checkEnv funcTable varTable initStmt =
  maybe [] (compileStatement checkEnv funcTable varTable) initStmt

-- for loop condition (faut check avant chaque iteration)
compileForCondition :: CheckEnv -> FuncTable -> VarTable -> Maybe Expr -> [IR.Instruction]
compileForCondition checkEnv funcTable varTable Nothing =
  [IR.PushBool True]  -- Alors infinite (void) si y a pas de conditions
compileForCondition checkEnv funcTable varTable (Just cond) =
  compileExpr checkEnv funcTable varTable cond

-- for loop update a relancer apres chaque iteration
compileForUpdate :: CheckEnv -> FuncTable -> VarTable -> Maybe Statement -> [IR.Instruction]
compileForUpdate checkEnv funcTable varTable updateStmt =
  maybe [] (compileStatement checkEnv funcTable varTable) updateStmt

-- calcule jump pour se barrer de la for loop
calculateForLoopEnd :: [IR.Instruction] -> [IR.Instruction] -> Int
calculateForLoopEnd bodyCode updateCode =
  length bodyCode + length updateCode + 2

-- on calcule pour revenir a la condition
calculateForLoopStart :: [IR.Instruction] -> [IR.Instruction] -> [IR.Instruction] -> Int
calculateForLoopStart conditionCode bodyCode updateCode =
  negate (length conditionCode + 1 + length bodyCode + length updateCode)
