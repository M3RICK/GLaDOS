# Type Checking

This page details the GLaDOS type checking system, which ensures type safety and correctness before code execution.

## Overview

The type checker is implemented in `src/Compiler.hs` and performs:

1. **Type inference**: Determine the type of every expression
2. **Type compatibility**: Verify operations use correct types
3. **Initialization tracking**: Ensure variables are initialized before use
4. **Return path analysis**: Verify all code paths return correct type
5. **Function validation**: Check function calls and signatures

## Type Checking Architecture

### Two-Pass Approach

The type checker uses a **two-pass algorithm**:

**Pass 1: Environment Building**
```haskell
buildGlobalEnv :: Program -> TypeEnv
buildGlobalEnv (Program funcs) = Map.fromList
  [ (funcName f, (funcReturnType f, map snd (funcParams f)))
  | f <- funcs
  ]
```

Collects all function signatures into a global environment for type checking function calls.

**Pass 2: Function Body Checking**
```haskell
checkFunction :: GlobalEnv -> FuncDef -> Either String ()
checkFunction globalEnv func = do
  let localEnv = initLocalEnv (funcParams func)
  let returnType = funcReturnType func
  checkStatements globalEnv localEnv returnType (funcBody func)
  verifyAllPathsReturn (funcBody func)
```

Validates each function body with proper type constraints.

## Type Environments

### Global Environment

Maps function names to their signatures:

```haskell
type GlobalEnv = Map String (Type, [Type])
-- Map from function name to (return type, parameter types)

-- Example:
-- "add" -> (TInt, [TInt, TInt])
-- "isPositive" -> (TBool, [TInt])
```

### Local Environment

Tracks local variables and their initialization status:

```haskell
type LocalEnv = Map String (Type, InitStatus)

data InitStatus
  = Initialized    -- Variable has a value
  | Uninitialized  -- Variable declared but not initialized

-- Example:
-- "x" -> (TInt, Initialized)
-- "y" -> (TBool, Uninitialized)
```

## Expression Type Checking

### Type Inference

Every expression has an inferred type:

```haskell
inferExprType :: GlobalEnv -> LocalEnv -> Expr -> Either String Type

-- Literal
inferExprType _ _ (Lit (IntLit _)) = Right TInt
inferExprType _ _ (Lit (BoolLit _)) = Right TBool

-- Variable
inferExprType _ env (Var name) =
  case Map.lookup name env of
    Nothing -> Left $ "Undefined variable '" ++ name ++ "'"
    Just (ty, Uninitialized) -> Left $ "Variable '" ++ name ++ "' used before initialization"
    Just (ty, Initialized) -> Right ty

-- Binary operation
inferExprType globalEnv env (BinOp op left right) = do
  leftType <- inferExprType globalEnv env left
  rightType <- inferExprType globalEnv env right
  checkBinOp op leftType rightType

-- Function call
inferExprType globalEnv env (Call fname args) = do
  (returnType, paramTypes) <- lookupFunction globalEnv fname
  argTypes <- mapM (inferExprType globalEnv env) args
  checkArgumentTypes fname paramTypes argTypes
  return returnType
```

### Binary Operator Type Rules

Different operators have different type requirements:

```haskell
checkBinOp :: BinOp -> Type -> Type -> Either String Type

-- Arithmetic operators: int × int → int
checkBinOp Add TInt TInt = Right TInt
checkBinOp Sub TInt TInt = Right TInt
checkBinOp Mul TInt TInt = Right TInt
checkBinOp Div TInt TInt = Right TInt
checkBinOp op TInt TInt = Left $ "Type error: arithmetic operation requires int operands"

-- Comparison operators: T × T → bool (same types)
checkBinOp Eq t1 t2 | t1 == t2 = Right TBool
checkBinOp Neq t1 t2 | t1 == t2 = Right TBool
checkBinOp Lt TInt TInt = Right TBool
checkBinOp Gt TInt TInt = Right TBool
checkBinOp Lte TInt TInt = Right TBool
checkBinOp Gte TInt TInt = Right TBool

-- Logical operators: bool × bool → bool
checkBinOp And TBool TBool = Right TBool
checkBinOp Or TBool TBool = Right TBool

-- Type mismatch
checkBinOp op t1 t2 = Left $ "Type error: incompatible types " ++ show t1 ++ " and " ++ show t2
```

## Statement Type Checking

### Variable Declaration

```haskell
checkVarDecl :: GlobalEnv -> LocalEnv -> Type -> String -> Maybe Expr -> Either String LocalEnv
checkVarDecl globalEnv env declaredType varName Nothing =
  -- Declaration without initialization
  Right $ Map.insert varName (declaredType, Uninitialized) env

checkVarDecl globalEnv env declaredType varName (Just initExpr) = do
  -- Declaration with initialization
  exprType <- inferExprType globalEnv env initExpr
  when (exprType /= declaredType) $
    Left $ "Type error: expected " ++ show declaredType ++ " but got " ++ show exprType
  Right $ Map.insert varName (declaredType, Initialized) env
```

**Example:**
```c
int x;           // OK: x is TInt, Uninitialized
int y = 42;      // OK: y is TInt, Initialized
int z = true;    // ERROR: expected int but got bool
```

### Assignment

```haskell
checkAssignment :: GlobalEnv -> LocalEnv -> String -> Expr -> Either String LocalEnv
checkAssignment globalEnv env varName expr = do
  (varType, _) <- lookupVariable env varName
  exprType <- inferExprType globalEnv env expr
  when (exprType /= varType) $
    Left $ "Type error: cannot assign " ++ show exprType ++ " to " ++ show varType
  -- Mark variable as initialized after assignment
  Right $ Map.insert varName (varType, Initialized) env
```

**Example:**
```c
int x;
x = 42;      // OK: x becomes Initialized
x = true;    // ERROR: cannot assign bool to int
```

### If Statement

```haskell
checkIf :: GlobalEnv -> LocalEnv -> Type -> Expr -> [Statement] -> Maybe [Statement] -> Either String ()
checkIf globalEnv env returnType condition thenBody elseBody = do
  -- Check condition is bool
  condType <- inferExprType globalEnv env condition
  when (condType /= TBool) $
    Left $ "Type error: if condition must be bool, got " ++ show condType

  -- Check then branch
  checkStatements globalEnv env returnType thenBody

  -- Check else branch if present
  case elseBody of
    Nothing -> Right ()
    Just stmts -> checkStatements globalEnv env returnType stmts
```

**Example:**
```c
if (x > 0) {        // OK: x > 0 is bool
    return 1;
}

if (x) {            // ERROR: x is int, not bool
    return 1;
}
```

### While Loop

```haskell
checkWhile :: GlobalEnv -> LocalEnv -> Type -> Expr -> [Statement] -> Either String ()
checkWhile globalEnv env returnType condition body = do
  -- Check condition is bool
  condType <- inferExprType globalEnv env condition
  when (condType /= TBool) $
    Left $ "Type error: while condition must be bool, got " ++ show condType

  -- Check body
  checkStatements globalEnv env returnType body
```

**Example:**
```c
while (x > 0) {     // OK: x > 0 is bool
    x = x - 1;
}

while (x) {         // ERROR: x is int, not bool
    x = x - 1;
}
```

### Return Statement

```haskell
checkReturn :: GlobalEnv -> LocalEnv -> Type -> Expr -> Either String ()
checkReturn globalEnv env expectedType expr = do
  actualType <- inferExprType globalEnv env expr
  when (actualType /= expectedType) $
    Left $ "Return type mismatch: expected " ++ show expectedType ++ " but got " ++ show actualType
```

**Example:**
```c
int getValue() {
    return 42;      // OK: returns int
    return true;    // ERROR: expected int but got bool
}
```

## Function Call Validation

### Argument Type Checking

```haskell
checkFunctionCall :: GlobalEnv -> LocalEnv -> String -> [Expr] -> Either String Type
checkFunctionCall globalEnv env funcName args = do
  -- Lookup function signature
  (returnType, paramTypes) <- case Map.lookup funcName globalEnv of
    Nothing -> Left $ "Undefined function '" ++ funcName ++ "'"
    Just sig -> Right sig

  -- Check argument count
  when (length args /= length paramTypes) $
    Left $ "Function '" ++ funcName ++ "' expects " ++ show (length paramTypes) ++
           " arguments but got " ++ show (length args)

  -- Infer argument types
  argTypes <- mapM (inferExprType globalEnv env) args

  -- Check each argument type
  zipWithM_ (checkArgType funcName) (zip [1..] paramTypes) argTypes

  return returnType

checkArgType :: String -> (Int, Type) -> Type -> Either String ()
checkArgType funcName (idx, expected) actual =
  when (expected /= actual) $
    Left $ "Type mismatch in function '" ++ funcName ++ "' argument " ++ show idx ++
           ": expected " ++ show expected ++ " but got " ++ show actual
```

**Example:**
```c
int add(int a, int b) {
    return a + b;
}

int main() {
    return add(5, 3);       // OK: 2 int arguments
    return add(5);          // ERROR: expects 2 arguments, got 1
    return add(true, 5);    // ERROR: argument 1 expected int but got bool
}
```

## Initialization Tracking

### Variable Usage Before Initialization

The type checker tracks whether variables are initialized:

```haskell
-- When variable is declared without initialization
Map.insert varName (varType, Uninitialized) env

-- When variable is used
case Map.lookup varName env of
  Just (ty, Uninitialized) ->
    Left $ "Variable '" ++ varName ++ "' used before initialization"
  Just (ty, Initialized) ->
    Right ty

-- When variable is assigned
Map.insert varName (varType, Initialized) env
```

**Example:**
```c
int x;
return x;       // ERROR: x used before initialization

int y;
y = 10;
return y;       // OK: y initialized before use

int z = 5;
return z;       // OK: z initialized at declaration
```

### Initialization Flow Analysis

The checker tracks initialization through control flow:

```c
int x;
if (condition) {
    x = 5;           // x initialized in this branch
} else {
    x = 10;          // x initialized in this branch
}
return x;            // OK: x initialized on all paths

int y;
if (condition) {
    y = 5;           // y initialized only here
}
return y;            // ERROR: y might be uninitialized
```

## Return Path Analysis

### Verifying All Paths Return

Every non-void function must return on all code paths:

```haskell
verifyAllPathsReturn :: [Statement] -> Either String ()
verifyAllPathsReturn stmts =
  unless (allPathsReturn stmts) $
    Left "Function must return a value on all code paths"

allPathsReturn :: [Statement] -> Bool
allPathsReturn [] = False
allPathsReturn stmts = any isReturn stmts || hasIfElseReturn stmts

isReturn :: Statement -> Bool
isReturn (Return _) = True
isReturn _ = False

hasIfElseReturn :: [Statement] -> Bool
hasIfElseReturn stmts = any checkIfElse stmts
  where
    checkIfElse (If _ thenBody (Just elseBody)) =
      allPathsReturn thenBody && allPathsReturn elseBody
    checkIfElse _ = False
```

**Example:**
```c
// ✗ ERROR: Missing return
int bad1(int x) {
    if (x > 0) {
        return 1;
    }
    // No return here!
}

// ✗ ERROR: Empty body
int bad2() {
}

// ✓ OK: Return after if
int good1(int x) {
    if (x > 0) {
        return 1;
    }
    return 0;
}

// ✓ OK: Both branches return
int good2(int x) {
    if (x > 0) {
        return 1;
    } else {
        return 0;
    }
}
```

## Type Error Messages

### Error Message Format

Type errors include detailed information:

```haskell
typeError :: Int -> String -> String
typeError line msg = "Type error at line " ++ show line ++ ": " ++ msg
```

**Examples:**
```
Type error at line 5: expected int but got bool
Type error at line 10: variable 'x' used before initialization
Type error at line 15: undefined function 'foo'
Type error at line 20: function 'add' expects 2 arguments but got 1
Type error at line 25: type mismatch in function argument: expected int but got bool
Return type mismatch at line 30: expected int but got bool
```

### Context-Specific Errors

Different error messages for different contexts:

- **Variable errors**: "Undefined variable 'x'", "Variable 'x' used before initialization"
- **Type mismatches**: "expected int but got bool"
- **Function errors**: "Undefined function 'foo'", "Function 'add' expects 2 arguments but got 1"
- **Return errors**: "Return type mismatch", "Function must return a value on all code paths"
- **Operator errors**: "Type error in arithmetic: expected int but got bool"

## Type Checking Algorithm

### Complete Algorithm

```haskell
typeCheck :: Program -> Either String ()
typeCheck program@(Program funcs) = do
  -- Pass 1: Build global environment
  let globalEnv = buildGlobalEnv program

  -- Verify main function exists
  unless (Map.member "main" globalEnv) $
    Left "Program must have a 'main' function"

  -- Pass 2: Check each function
  mapM_ (checkFunction globalEnv) funcs

checkFunction :: GlobalEnv -> FuncDef -> Either String ()
checkFunction globalEnv func = do
  -- Initialize local environment with parameters
  let localEnv = Map.fromList
        [ (name, (ty, Initialized))
        | (name, ty) <- funcParams func
        ]

  -- Check function body
  checkStatements globalEnv localEnv (funcReturnType func) (funcBody func)

  -- Verify all paths return
  verifyAllPathsReturn (funcBody func)

checkStatements :: GlobalEnv -> LocalEnv -> Type -> [Statement] -> Either String ()
checkStatements globalEnv env returnType stmts =
  foldM_ (checkStatement globalEnv returnType) env stmts

checkStatement :: GlobalEnv -> Type -> LocalEnv -> Statement -> Either String LocalEnv
checkStatement globalEnv returnType env stmt =
  case stmt of
    VarDecl ty name init ->
      checkVarDecl globalEnv env ty name init

    Assign name expr ->
      checkAssignment globalEnv env name expr

    If cond thenBody elseBody -> do
      checkIf globalEnv env returnType cond thenBody elseBody
      return env

    While cond body -> do
      checkWhile globalEnv env returnType cond body
      return env

    Return expr -> do
      checkReturn globalEnv env returnType expr
      return env

    ExprStmt expr -> do
      _ <- inferExprType globalEnv env expr
      return env
```

## Type Checking Performance

### Complexity Analysis

- **Time Complexity**: O(n) where n is the size of the AST
  - Each expression/statement is visited once
  - Environment lookups are O(log n) with Map
- **Space Complexity**: O(n) for environments and call stack

### Optimization Opportunities

- **Environment Sharing**: Reuse environments when possible
- **Early Exit**: Stop at first error for faster failure
- **Lazy Evaluation**: Haskell's lazy evaluation helps

## Testing Type Checker

### Test Cases

```haskell
-- Test: Type mismatch
testTypeMismatch = "int main() { int x = true; return x; }"

-- Test: Undefined variable
testUndefinedVar = "int main() { return x; }"

-- Test: Uninitialized variable
testUninitialized = "int main() { int x; return x; }"

-- Test: Missing return
testMissingReturn = "int getValue(int x) { if (x > 0) { return 1; } }"

-- Test: Function call errors
testWrongArgCount = "int add(int a, int b) { return a + b; } int main() { return add(5); }"
testWrongArgType = "int add(int a, int b) { return a + b; } int main() { return add(true, 5); }"
```

### Running Tests

```bash
stack test --test-arguments="--match TypeChecker"
```

## Next Steps

After type checking succeeds, the program moves to:

- [IR Generation](./ir-generation.md): Convert type-safe AST to bytecode
- [VM Execution](./vm-execution.md): Execute the bytecode

See also:
- [Type System](../language-reference/types.md): Language-level type rules
- [Error Messages](../user-guide/error-messages.md): User-facing error documentation
