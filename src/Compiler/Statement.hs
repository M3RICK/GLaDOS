module Compiler.Statement (compileStatement, compileStatements) where

import qualified IR.Types as IR
import AST.AST
import Compiler.Environment
import Compiler.Expr
import Security.Types (CheckEnv)
import Error.Types (CompilerError)

-- Statement into IR instructions
compileStatement :: CheckEnv -> FuncTable -> VarTable -> Statement -> Either CompilerError [IR.Instruction]
compileStatement checkEnv funcTable varTable stmt = case stmt of
  Decl _ name maybeExpr -> compileDecl checkEnv funcTable varTable name maybeExpr
  Assign name expr -> compileAssign checkEnv funcTable varTable name expr
  If cond thenBody elseBody -> compileIf checkEnv funcTable varTable cond thenBody elseBody
  While cond body -> compileWhile checkEnv funcTable varTable cond body
  For initStmt cond update body -> compileFor checkEnv funcTable varTable initStmt cond update body
  Return expr -> compileReturn checkEnv funcTable varTable expr
  ExprStmt expr -> compileExprStmt checkEnv funcTable varTable expr

-- Variable declaration
compileDecl :: CheckEnv -> FuncTable -> VarTable -> String -> Maybe Expr -> Either CompilerError [IR.Instruction]
compileDecl checkEnv funcTable varTable name maybeExpr =
  case maybeExpr of
    Nothing -> Right []
    Just expr -> compileDeclWithInit checkEnv funcTable varTable name expr

-- Declaration with initialization
compileDeclWithInit :: CheckEnv -> FuncTable -> VarTable -> String -> Expr -> Either CompilerError [IR.Instruction]
compileDeclWithInit checkEnv funcTable varTable name expr =
  case compileExpr checkEnv funcTable varTable expr of
    Left err -> Left err
    Right exprCode -> Right (exprCode ++ [IR.SetLocal (lookupVar varTable name)])

-- Compile assignment statement
compileAssign :: CheckEnv -> FuncTable -> VarTable -> String -> Expr -> Either CompilerError [IR.Instruction]
compileAssign checkEnv funcTable varTable name expr =
  case compileExpr checkEnv funcTable varTable expr of
    Left err -> Left err
    Right exprCode -> Right (exprCode ++ [IR.SetLocal (lookupVar varTable name)])

-- | Compile if statement
compileIf :: CheckEnv -> FuncTable -> VarTable -> Expr -> [Statement] -> Maybe [Statement] -> Either CompilerError [IR.Instruction]
compileIf checkEnv funcTable varTable cond thenBody Nothing =
  compileSimpleIf checkEnv funcTable varTable cond thenBody
compileIf checkEnv funcTable varTable cond thenBody (Just elseBody) =
  compileIfElse checkEnv funcTable varTable cond thenBody elseBody

-- if (cond) { then }
compileSimpleIf :: CheckEnv -> FuncTable -> VarTable -> Expr -> [Statement] -> Either CompilerError [IR.Instruction]
compileSimpleIf checkEnv funcTable varTable cond thenBody =
  case compileExpr checkEnv funcTable varTable cond of
    Left err -> Left err
    Right conditionCode -> case compileStatements checkEnv funcTable varTable thenBody of
      Left err -> Left err
      Right thenCode ->
        let skipDistance = length thenCode + 1
        in Right (conditionCode ++ [IR.JumpIfFalse skipDistance] ++ thenCode)

-- if (cond) { then } else { else }
compileIfElse :: CheckEnv -> FuncTable -> VarTable -> Expr -> [Statement] -> [Statement] -> Either CompilerError [IR.Instruction]
compileIfElse checkEnv funcTable varTable cond thenBody elseBody =
  case compileExpr checkEnv funcTable varTable cond of
    Left err -> Left err
    Right conditionCode -> case compileStatements checkEnv funcTable varTable thenBody of
      Left err -> Left err
      Right thenCode -> case compileStatements checkEnv funcTable varTable elseBody of
        Left err -> Left err
        Right elseCode ->
          let jumpToElse = calculateJumpToElse thenCode
              jumpToEnd = calculateJumpToEnd elseCode
          in Right (conditionCode ++ [IR.JumpIfFalse jumpToElse] ++ thenCode ++ [IR.Jump jumpToEnd] ++ elseCode)

-- | Calculate jump distance to skip then branch and reach else branch
calculateJumpToElse :: [IR.Instruction] -> Int
calculateJumpToElse thenCode = length thenCode + 2  -- on saute le then et en plus de ca on unconditionnal jump

-- | Calculate jump distance to skip else branch and reach end
calculateJumpToEnd :: [IR.Instruction] -> Int
calculateJumpToEnd elseCode = length elseCode + 1   -- rdv apres le else

-- while (cond) { body }
compileWhile :: CheckEnv -> FuncTable -> VarTable -> Expr -> [Statement] -> Either CompilerError [IR.Instruction]
compileWhile checkEnv funcTable varTable cond body =
  case compileExpr checkEnv funcTable varTable cond of
    Left err -> Left err
    Right conditionCode -> case compileStatements checkEnv funcTable varTable body of
      Left err -> Left err
      Right bodyCode ->
        let jumpToEnd = calculateJumpToLoopEnd bodyCode
            jumpBackToStart = calculateJumpBackToLoopStart conditionCode bodyCode
        in Right (conditionCode ++ [IR.JumpIfFalse jumpToEnd] ++ bodyCode ++ [IR.Jump jumpBackToStart])

calculateJumpToLoopEnd :: [IR.Instruction] -> Int
calculateJumpToLoopEnd bodyCode = length bodyCode + 2

-- Calcule combien d etapes il doit revenir en arriere
calculateJumpBackToLoopStart :: [IR.Instruction] -> [IR.Instruction] -> Int
calculateJumpBackToLoopStart conditionCode bodyCode =
  negate totalLoopSize  -- Retourne a la condition
  where
    totalLoopSize = length conditionCode + 1 + length bodyCode

-- return statement
compileReturn :: CheckEnv -> FuncTable -> VarTable -> Expr -> Either CompilerError [IR.Instruction]
compileReturn checkEnv funcTable varTable expr =
  compileExprAndReturn checkEnv funcTable varTable expr

compileExprAndReturn :: CheckEnv -> FuncTable -> VarTable -> Expr -> Either CompilerError [IR.Instruction]
compileExprAndReturn checkEnv funcTable varTable expr =
  case compileExpr checkEnv funcTable varTable expr of
    Left err -> Left err
    Right exprCode -> Right (exprCode ++ [IR.Return])

-- expression statement (result discarded)
compileExprStmt :: CheckEnv -> FuncTable -> VarTable -> Expr -> Either CompilerError [IR.Instruction]
compileExprStmt checkEnv funcTable varTable expr =
  compileExprAndDiscard checkEnv funcTable varTable expr

-- | Compile expression and discard result (for side effects only)
compileExprAndDiscard :: CheckEnv -> FuncTable -> VarTable -> Expr -> Either CompilerError [IR.Instruction]
compileExprAndDiscard checkEnv funcTable varTable expr =
  case compileExpr checkEnv funcTable varTable expr of
    Left err -> Left err
    Right exprCode -> Right (exprCode ++ [IR.Pop])

-- Compile multiple statements
compileStatements :: CheckEnv -> FuncTable -> VarTable -> [Statement] -> Either CompilerError [IR.Instruction]
compileStatements checkEnv funcTable varTable stmts =
  case mapM (compileStatement checkEnv funcTable varTable) stmts of
    Left err -> Left err
    Right stmtCodes -> Right (concat stmtCodes)

-- for (init; cond; update) { body }
compileFor :: CheckEnv -> FuncTable -> VarTable -> Maybe Statement -> Maybe Expr -> Maybe Statement -> [Statement] -> Either CompilerError [IR.Instruction]
compileFor checkEnv funcTable varTable initStmt condExpr updateStmt body =
  case compileForInit checkEnv funcTable varTable initStmt of
    Left err -> Left err
    Right initCode -> case compileForCondition checkEnv funcTable varTable condExpr of
      Left err -> Left err
      Right conditionCode -> case compileStatements checkEnv funcTable varTable body of
        Left err -> Left err
        Right bodyCode -> case compileForUpdate checkEnv funcTable varTable updateStmt of
          Left err -> Left err
          Right updateCode ->
            let jumpToEnd = calculateForLoopEnd bodyCode updateCode
                jumpBackToStart = calculateForLoopStart conditionCode bodyCode updateCode
            in Right (initCode ++ conditionCode ++ [IR.JumpIfFalse jumpToEnd] ++ bodyCode ++ updateCode ++ [IR.Jump jumpBackToStart])

-- for loop init (runs once before loop)
compileForInit :: CheckEnv -> FuncTable -> VarTable -> Maybe Statement -> Either CompilerError [IR.Instruction]
compileForInit checkEnv funcTable varTable initStmt =
  case initStmt of
    Nothing -> Right []
    Just stmt -> compileStatement checkEnv funcTable varTable stmt

-- for loop condition (faut check avant chaque iteration)
compileForCondition :: CheckEnv -> FuncTable -> VarTable -> Maybe Expr -> Either CompilerError [IR.Instruction]
compileForCondition _ _ _ Nothing =
  Right [IR.PushBool True]  -- Alors infinite (void) si y a pas de conditions
compileForCondition checkEnv funcTable varTable (Just cond) =
  compileExpr checkEnv funcTable varTable cond

-- for loop update a relancer apres chaque iteration
compileForUpdate :: CheckEnv -> FuncTable -> VarTable -> Maybe Statement -> Either CompilerError [IR.Instruction]
compileForUpdate checkEnv funcTable varTable updateStmt =
  case updateStmt of
    Nothing -> Right []
    Just stmt -> compileStatement checkEnv funcTable varTable stmt

-- calcule jump pour se barrer de la for loop
calculateForLoopEnd :: [IR.Instruction] -> [IR.Instruction] -> Int
calculateForLoopEnd bodyCode updateCode =
  length bodyCode + length updateCode + 2

-- on calcule pour revenir a la condition
calculateForLoopStart :: [IR.Instruction] -> [IR.Instruction] -> [IR.Instruction] -> Int
calculateForLoopStart conditionCode bodyCode updateCode =
  negate (length conditionCode + 1 + length bodyCode + length updateCode)
