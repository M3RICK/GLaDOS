module Compiler.Environment where

import qualified Data.Map as M
import AST.AST

-- nom d’une variable à son indice local
type VarTable = M.Map String Int

-- nom d’une fonction à son indice
type FuncTable = M.Map String Int

-- { "x" -> 0, "y" -> 1, "z" -> 2 }
makeParamTable :: [Parameter] -> VarTable
makeParamTable params =
  M.fromList $ zip (map paramName params) [0..]

-- On file un indice UNIQUE a chaque fonction
makeFuncTable :: [Function] -> FuncTable
makeFuncTable funcs =
  M.fromList $ zip (map fName funcs) [0..]

-- On cherche indice de main, si rien ducp 0
findMainIndex :: [Function] -> Int
findMainIndex funcs =
  case [i | (i, f) <- zip [0..] funcs, fName f == "main"] of
    (idx:_) -> idx
    [] -> 0

-- Recup toutes les declarations de variables locales
collectLocalDecls :: [Statement] -> [(String, Type)]
collectLocalDecls stmts = concatMap extractDecl stmts

--   On gère les cas de base, les 'if', les 'while', et les 'for'.
extractDecl :: Statement -> [(String, Type)]
extractDecl (Decl typ name _) = extractSimpleDecl typ name
extractDecl (If _ thenBody maybeElse) = extractIfDecls thenBody maybeElse
extractDecl (While _ body) = extractWhileDecls body
extractDecl (For maybeInit _ _ body) = extractForDecls maybeInit body
extractDecl _ = []  -- les autres instructions ne déclarent rien

-- Extract a simple variable declaration
extractSimpleDecl :: Type -> String -> [(String, Type)]
extractSimpleDecl typ name = [(name, typ)]

extractIfDecls :: [Statement] -> Maybe [Statement] -> [(String, Type)]
extractIfDecls thenBody maybeElse = collectFromBranches thenBody maybeElse

extractWhileDecls :: [Statement] -> [(String, Type)]
extractWhileDecls body = collectLocalDecls body

extractForDecls :: Maybe Statement -> [Statement] -> [(String, Type)]
extractForDecls maybeInit body =
  maybe [] extractDecl maybeInit ++ collectLocalDecls body

-- | Récupère les déclarations dans les deux branches d un if
collectFromBranches :: [Statement] -> Maybe [Statement] -> [(String, Type)]
collectFromBranches thenBody maybeElse =
  collectLocalDecls thenBody ++ maybe [] collectLocalDecls maybeElse

-- complete variable table (params + locals)
buildVarTable :: [Parameter] -> [Statement] -> VarTable
buildVarTable params body = M.union paramTable localTable
  where
    paramTable = makeParamTable params
    localTable = makeLocalTable params body

makeLocalTable :: [Parameter] -> [Statement] -> VarTable
makeLocalTable params body =
  M.fromList $ zip localVarNames localIndices
  where
    localVars = collectLocalDecls body
    localVarNames = map fst localVars
    localIndices = [paramCount..]
    paramCount = length params

-- | Look up variable index in variable table
lookupVar :: VarTable -> String -> Int
lookupVar varTable name =
  case M.lookup name varTable of
    Just idx -> idx
    Nothing -> error $ "Variable not in table: " ++ name

-- | Look up function index in function table
lookupFunc :: FuncTable -> String -> Int
lookupFunc funcTable name =
  case M.lookup name funcTable of
    Just idx -> idx
    Nothing -> error $ "Function not in table: " ++ name
