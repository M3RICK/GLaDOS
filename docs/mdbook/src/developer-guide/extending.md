# Extending the Language

This guide helps developers extend GLaDOS with new features, operators, types, or optimizations.

## Overview

GLaDOS has a modular architecture that makes it relatively easy to extend:

```
Source Code
    ↓
AST (AST.hs)
    ↓
Parser (Parser.hs)
    ↓
Security (Security.hs)
    ↓
Type Checker (Compiler.hs)
    ↓
IR Generator (Compiler.hs)
    ↓
VM (VM.hs)
```

To add a feature, you typically need to modify multiple stages.

## Project Structure

```
glados/
├── src/
│   ├── AST.hs           # Abstract Syntax Tree data types
│   ├── Parser.hs        # Lexer and parser
│   ├── Security.hs      # Security analysis
│   ├── Compiler.hs      # Type checking + IR generation
│   ├── IR.hs            # IR data types
│   ├── VM.hs            # Virtual machine
│   └── Main.hs          # Entry point
├── tests/
│   ├── dotC/            # Test programs
│   └── Spec.hs          # Hspec test suite
├── Makefile             # Build system
├── package.yaml         # Hpack configuration
└── stack.yaml           # Stack configuration
```

## Adding a New Operator

Let's add the modulo operator `%` as an example.

### Step 1: Update AST

**File**: `src/AST.hs`

Add the operator to `BinOp`:

```haskell
data BinOp
  = Add | Sub | Mul | Div
  | Mod  -- NEW: Modulo operator
  | Eq | Neq | Lt | Gt | Lte | Gte
  | And | Or
  deriving (Show, Eq)
```

### Step 2: Update Parser

**File**: `src/Parser.hs`

Add to operator table:

```haskell
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ binary "*" Mul, binary "/" Div, binary "%" Mod ]  -- Add Mod here
  , [ binary "+" Add, binary "-" Sub ]
  , -- ... rest unchanged
  ]
```

### Step 3: Update Type Checker

**File**: `src/Compiler.hs`

Add type rule for modulo:

```haskell
checkBinOp :: BinOp -> Type -> Type -> Either String Type
-- ... existing cases ...
checkBinOp Mod TInt TInt = Right TInt  -- int % int → int
checkBinOp Mod t1 t2 = Left $ "Type error in modulo: expected int but got " ++ show t1 ++ " and " ++ show t2
```

### Step 4: Update IR

**File**: `src/IR.hs`

Add IR instruction:

```haskell
data Instruction
  = -- ... existing ...
  | Add | Sub | Mul | Div
  | Mod  -- NEW
  | -- ... rest ...
  deriving (Show, Eq)
```

### Step 5: Update IR Generator

**File**: `src/Compiler.hs`

Add compilation case:

```haskell
compileBinOp :: BinOp -> Instruction
-- ... existing cases ...
compileBinOp Mod = Mod
```

### Step 6: Update VM

**File**: `src/VM.hs`

Add execution logic:

```haskell
executeInstruction :: Instruction -> VMState -> Either String VMState
-- ... existing cases ...
executeInstruction Mod state =
  case valueStack state of
    (IntVal b : IntVal a : rest) ->
      if b == 0
        then Left "Runtime error: Modulo by zero"
        else Right state
          { valueStack = IntVal (a `mod` b) : rest
          , pc = pc state + 1
          }
    _ -> Left "Runtime error: Type error in modulo"
```

### Step 7: Add Tests

**File**: `tests/Spec.hs` or create new test file

```haskell
describe "Modulo operator" $ do
  it "computes 10 % 3" $ do
    let program = "int main() { return 10 % 3; }"
    runGLaDOS program `shouldBe` Right 1

  it "detects modulo by zero" $ do
    let program = "int main() { return 10 % 0; }"
    runGLaDOS program `shouldSatisfy` isLeft
```

### Step 8: Build and Test

```bash
stack build
stack test
./glados < test.c
```

Done! You've added a new operator.

## Adding a New Type

Let's add a `string` type as an example (partial implementation).

### Step 1: Update AST

**File**: `src/AST.hs`

```haskell
data Type
  = TInt
  | TBool
  | TString  -- NEW
  deriving (Eq, Show)

data Literal
  = IntLit Int64
  | BoolLit Bool
  | StringLit String  -- NEW
  deriving (Show, Eq)
```

### Step 2: Update Parser

**File**: `src/Parser.hs`

```haskell
-- Add string type keyword
typeParser :: Parser Type
typeParser = choice
  [ TInt <$ keyword "int"
  , TBool <$ keyword "bool"
  , TString <$ keyword "string"  -- NEW
  ]

-- Add string literal parsing
stringLiteral :: Parser Literal
stringLiteral = StringLit <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))

literal :: Parser Literal
literal = choice
  [ IntLit <$> integer
  , BoolLit True <$ keyword "true"
  , BoolLit False <$ keyword "false"
  , stringLiteral  -- NEW
  ]
```

### Step 3: Update Type Checker

**File**: `src/Compiler.hs`

```haskell
-- String concatenation
checkBinOp Add TString TString = Right TString  -- string + string → string

-- String comparison
checkBinOp Eq TString TString = Right TBool
checkBinOp Neq TString TString = Right TBool
```

### Step 4: Update IR and VM

Add `StringVal` to IR values and handle in VM.

### Step 5: Add Standard Library Functions

Create string manipulation functions:

```c
int strlen(string s);
string concat(string a, string b);
string substr(string s, int start, int len);
```

## Adding a New Statement

Let's add a `for` loop as an example.

### Step 1: Update AST

**File**: `src/AST.hs`

```haskell
data Statement
  = -- ... existing ...
  | For Statement Expr Statement [Statement]  -- init, condition, update, body
```

### Step 2: Update Parser

**File**: `src/Parser.hs`

```haskell
forStatement :: Parser Statement
forStatement = do
  keyword "for"
  symbol "("
  init <- statement  -- Initialization
  symbol ";"
  cond <- expr       -- Condition
  symbol ";"
  update <- statement  -- Update
  symbol ")"
  body <- braces (many statement)
  return $ For init cond update body
```

### Step 3: Update Type Checker

**File**: `src/Compiler.hs`

```haskell
checkStatement globalEnv returnType env (For init cond update body) = do
  -- Check init statement
  env' <- checkStatement globalEnv returnType env init

  -- Check condition is bool
  condType <- inferExprType globalEnv env' cond
  when (condType /= TBool) $
    Left "For loop condition must be bool"

  -- Check update statement
  _ <- checkStatement globalEnv returnType env' update

  -- Check body
  checkStatements globalEnv env' returnType body

  return env
```

### Step 4: Update IR Generator

**File**: `src/Compiler.hs`

```haskell
compileStatement (For init cond update body) =
  let startLabel = genLabel "for_start"
      endLabel = genLabel "for_end"
  in
    compileStatement init ++           -- Execute init
    [Label startLabel] ++
    compileExpr cond ++               -- Check condition
    [JumpIfFalse endLabel] ++
    concatMap compileStatement body ++ -- Execute body
    compileStatement update ++        -- Execute update
    [Jump startLabel] ++
    [Label endLabel]
```

No VM changes needed (uses existing instructions).

## Adding Optimizations

### Constant Folding

**File**: `src/Compiler.hs`

```haskell
-- Optimize expressions during compilation
optimizeExpr :: Expr -> Expr
optimizeExpr (BinOp Add (Lit (IntLit a)) (Lit (IntLit b))) =
  Lit (IntLit (a + b))  -- Fold 5 + 3 → 8

optimizeExpr (BinOp Mul (Lit (IntLit 0)) _) =
  Lit (IntLit 0)  -- x * 0 → 0

optimizeExpr (BinOp Mul _ (Lit (IntLit 0))) =
  Lit (IntLit 0)  -- 0 * x → 0

optimizeExpr e = e  -- No optimization
```

### Dead Code Elimination

```haskell
removeDeadCode :: [Statement] -> [Statement]
removeDeadCode [] = []
removeDeadCode (Return e : _) = [Return e]  -- Everything after return is dead
removeDeadCode (s : ss) = s : removeDeadCode ss
```

### Peephole Optimization

**File**: `src/Compiler.hs`

```haskell
optimizeIR :: [Instruction] -> [Instruction]
optimizeIR (Push a : Push b : Add : rest) =
  -- Fold constant addition
  case (a, b) of
    (IntVal x, IntVal y) -> Push (IntVal (x + y)) : optimizeIR rest
    _ -> Push a : Push b : Add : optimizeIR rest

optimizeIR (Push _ : Pop : rest) =
  -- Remove push followed by pop
  optimizeIR rest

optimizeIR (inst : rest) = inst : optimizeIR rest
optimizeIR [] = []
```

## Adding Built-in Functions

### Step 1: Define in Global Environment

**File**: `src/Compiler.hs`

```haskell
builtinFunctions :: Map String (Type, [Type])
builtinFunctions = Map.fromList
  [ ("print", (TInt, [TInt]))        -- int print(int x)
  , ("abs", (TInt, [TInt]))          -- int abs(int x)
  , ("max", (TInt, [TInt, TInt]))    -- int max(int a, int b)
  ]

buildGlobalEnv :: Program -> GlobalEnv
buildGlobalEnv (Program funcs) =
  Map.union builtinFunctions $ Map.fromList
    [ (funcName f, (funcReturnType f, map snd (funcParams f)))
    | f <- funcs
    ]
```

### Step 2: Implement in VM

**File**: `src/VM.hs`

```haskell
executeInstruction (Call fname arity) state
  | fname `elem` builtinNames = executeBuiltin fname arity state
  | otherwise = -- ... normal function call

executeBuiltin :: String -> Int -> VMState -> Either String VMState
executeBuiltin "print" 1 state =
  case valueStack state of
    (IntVal x : rest) -> do
      -- In real implementation, would print x
      Right state
        { valueStack = IntVal x : rest  -- Return the value
        , pc = pc state + 1
        }
    _ -> Left "print: type error"

executeBuiltin "abs" 1 state =
  case valueStack state of
    (IntVal x : rest) -> Right state
      { valueStack = IntVal (abs x) : rest
      , pc = pc state + 1
      }
    _ -> Left "abs: type error"
```

## Testing Your Extensions

### Unit Tests

Add tests for each component:

```haskell
-- Test parser
describe "Parser extensions" $ do
  it "parses modulo operator" $ do
    parseExpr "10 % 3" `shouldBe` Right (BinOp Mod (Lit 10) (Lit 3))

-- Test type checker
describe "Type checker extensions" $ do
  it "type checks modulo" $ do
    typeCheck "int main() { return 10 % 3; }" `shouldBe` Right ()

-- Test VM
describe "VM extensions" $ do
  it "executes modulo" $ do
    execute [Push 10, Push 3, Mod] `shouldBe` Right (IntVal 1)
```

### Integration Tests

Create test programs in `tests/dotC/`:

```c
// tests/dotC/modulo.c
int main() {
    int x = 10;
    int y = 3;
    return x % y;
}
```

Run with:
```bash
./glados < tests/dotC/modulo.c
```

## Best Practices

### 1. Follow Existing Patterns

Look at existing features and follow the same structure.

### 2. Update All Stages

Don't forget any stage:
- AST
- Parser
- Type Checker
- IR
- VM

### 3. Add Comprehensive Tests

Test:
- Happy path
- Error cases
- Edge cases

### 4. Document Your Changes

Add documentation for new features:
- Update grammar.md
- Update syntax.md
- Add examples

### 5. Maintain Type Safety

Ensure new features maintain GLaDOS's safety guarantees.

## Common Extensions

### Arrays

Add array types, indexing, and bounds checking.

### Strings

Add string type with concatenation and standard library.

### Structs

Add user-defined types with fields.

### First-Class Functions

Add function pointers and higher-order functions.

### Module System

Add import/export for multi-file programs.

## Contributing

To contribute to GLaDOS:

1. **Fork the repository**
2. **Create a feature branch**
   ```bash
   git checkout -b feature/my-feature
   ```
3. **Make your changes**
4. **Add tests**
5. **Run test suite**
   ```bash
   stack test
   ```
6. **Create pull request**

## Resources

- **Source code**: Study existing implementations
- **Tests**: Look at `tests/` for examples
- **Parser docs**: Megaparsec documentation
- **Haskell docs**: Haskell wiki and tutorials
- **Compiler books**: "Crafting Interpreters", "Modern Compiler Implementation"

## Example Projects

Ideas for extending GLaDOS:

1. **Add arrays with bounds checking**
2. **Implement string manipulation**
3. **Add struct types**
4. **Implement first-class functions**
5. **Add pattern matching**
6. **Create standard library**
7. **Build native code backend**
8. **Add garbage collection**
9. **Implement concurrency**
10. **Create IDE plugin**

## Conclusion

GLaDOS's simple architecture makes it an excellent platform for learning about:

- Parsing with parser combinators
- Type systems and type checking
- IR design and code generation
- Virtual machine implementation
- Language security features

Have fun extending GLaDOS!
