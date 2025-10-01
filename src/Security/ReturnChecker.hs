module Security.ReturnChecker where

import AST.AST

-- Check if statement list definitely returns
listHasReturn :: [Statement] -> Bool
listHasReturn [] = False
listHasReturn (stmt:rest) =
  statementHasReturn stmt || listHasReturn rest

-- Check if single statement definitely returns
statementHasReturn :: Statement -> Bool
statementHasReturn (Return _) = True
statementHasReturn (If _ thenBranch (Just elseBranch)) =
  listHasReturn thenBranch && listHasReturn elseBranch
statementHasReturn _ = False
