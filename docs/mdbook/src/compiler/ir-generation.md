# IR Generation

> **WARNING**: This page is still being completed. Some of the information is false and filled with temporary dummy information. The actual IR syntax has changed significantly from what is documented here, as a custom WebAssembly-like format was created for this project.

This page details how GLaDOS translates the type-checked AST into Intermediate Representation (IR), a stack-based bytecode suitable for VM execution.

## Overview

IR generation is implemented in `src/Compiler.hs` and produces instructions defined in `src/IR.hs`. The IR is a **stack-based bytecode** that the GLaDOS virtual machine executes.

## IR Instruction Set

### Stack Operations

**`Push <value>`**
- Pushes a literal value onto the stack
- Example: `Push 42` pushes integer 42

**`Pop`**
- Pops the top value from the stack
- Used to discard unused results

**`PopN <count>`**
- Pops N values from the stack
- Used for cleaning up function parameters

### Variable Operations

**`Load <variable>`**
- Loads a variable's value onto the stack
- Example: `Load "x"` pushes value of x

**`Store <variable>`**
- Pops a value from stack and stores it in a variable
- Example: `Store "x"` sets x to top stack value

### Arithmetic Operations

All arithmetic instructions pop two operands from the stack and push the result:

**`Add`**
- Pops b, pops a, pushes (a + b)

**`Sub`**
- Pops b, pops a, pushes (a - b)

**`Mul`**
- Pops b, pops a, pushes (a * b)

**`Div`**
- Pops b, pops a, pushes (a / b)
- Runtime error if b is 0

### Comparison Operations

All comparison instructions pop two operands and push a boolean result:

**`Eq`**
- Equal: pushes (a == b)

**`Neq`**
- Not equal: pushes (a != b)

**`Lt`**
- Less than: pushes (a < b)

**`Gt`**
- Greater than: pushes (a > b)

**`Lte`**
- Less than or equal: pushes (a <= b)

**`Gte`**
- Greater than or equal: pushes (a >= b)

### Logical Operations

**`And`**
- Pops b, pops a, pushes (a && b)
- Short-circuit: if a is false, b is not evaluated

**`Or`**
- Pops b, pops a, pushes (a || b)
- Short-circuit: if a is true, b is not evaluated

### Control Flow Operations

**`Label <name>`**
- Marks a jump target
- Does not affect stack or execution

**`Jump <label>`**
- Unconditional jump to label
- Changes program counter

**`JumpIfFalse <label>`**
- Pops top of stack
- Jumps to label if value is false
- Otherwise continues to next instruction

### Function Operations

**`Call <function> <arity>`**
- Calls a function with given number of arguments
- Arguments are on the stack (topmost is last argument)
- Pushes return value onto stack

**`Return`**
- Returns from current function
- Top of stack is the return value
- Pops call frame

## IR Data Types

### Instruction Type

```haskell
data Instruction
  = Push Value                -- Push literal value
  | Pop                       -- Pop and discard
  | PopN Int                  -- Pop N values
  | Load String               -- Load variable
  | Store String              -- Store to variable
  | Add | Sub | Mul | Div     -- Arithmetic
  | Eq | Neq | Lt | Gt | Lte | Gte  -- Comparison
  | And | Or                  -- Logical
  | Jump String               -- Unconditional jump
  | JumpIfFalse String        -- Conditional jump
  | Call String Int           -- Function call
  | Return                    -- Return from function
  | Label String              -- Jump target
  deriving (Show, Eq)
```

### Value Type

```haskell
data Value
  = IntVal Int64              -- Integer value
  | BoolVal Bool              -- Boolean value
  deriving (Show, Eq)
```

## AST to IR Translation

### Expression Translation

```haskell
compileExpr :: Expr -> [Instruction]

-- Literal: push value
compileExpr (Lit (IntLit n)) = [Push (IntVal n)]
compileExpr (Lit (BoolLit b)) = [Push (BoolVal b)]

-- Variable: load value
compileExpr (Var name) = [Load name]

-- Binary operation: compile operands then apply operator
compileExpr (BinOp op left right) =
  compileExpr left ++
  compileExpr right ++
  [compileBinOp op]

-- Function call: compile arguments then call
compileExpr (Call fname args) =
  concatMap compileExpr args ++
  [Call fname (length args)]

compileBinOp :: BinOp -> Instruction
compileBinOp Add = Add
compileBinOp Sub = Sub
compileBinOp Mul = Mul
compileBinOp Div = Div
compileBinOp Eq = Eq
compileBinOp Neq = Neq
compileBinOp Lt = Lt
compileBinOp Gt = Gt
compileBinOp Lte = Lte
compileBinOp Gte = Gte
compileBinOp And = And
compileBinOp Or = Or
```

**Example:**
```c
int x = 5 + 3 * 2;
```

**IR:**
```
Push 5
Push 3
Push 2
Mul         # Stack: [5, 6]
Add         # Stack: [11]
Store "x"
```

### Statement Translation

```haskell
compileStatement :: Statement -> [Instruction]

-- Variable declaration with initialization
compileStatement (VarDecl _ name (Just expr)) =
  compileExpr expr ++
  [Store name]

-- Variable declaration without initialization
compileStatement (VarDecl _ name Nothing) =
  []  -- No IR needed, variable will be in environment

-- Assignment
compileStatement (Assign name expr) =
  compileExpr expr ++
  [Store name]

-- Return statement
compileStatement (Return expr) =
  compileExpr expr ++
  [Return]

-- Expression statement (result discarded)
compileStatement (ExprStmt expr) =
  compileExpr expr ++
  [Pop]
```

### Control Flow Translation

#### If Statement

```haskell
compileStatement (If cond thenBody elseBody) =
  let thenLabel = genLabel "then"
      elseLabel = genLabel "else"
      endLabel = genLabel "endif"
  in
    compileExpr cond ++
    [JumpIfFalse elseLabel] ++
    -- Then branch
    concatMap compileStatement thenBody ++
    [Jump endLabel] ++
    [Label elseLabel] ++
    -- Else branch
    maybe [] (concatMap compileStatement) elseBody ++
    [Label endLabel]
```

**Example:**
```c
if (x > 0) {
    return 1;
} else {
    return 0;
}
```

**IR:**
```
Load "x"
Push 0
Gt
JumpIfFalse else_1
  Push 1
  Return
Jump endif_1
Label else_1
  Push 0
  Return
Label endif_1
```

#### While Loop

```haskell
compileStatement (While cond body) =
  let startLabel = genLabel "while_start"
      endLabel = genLabel "while_end"
  in
    [Label startLabel] ++
    compileExpr cond ++
    [JumpIfFalse endLabel] ++
    concatMap compileStatement body ++
    [Jump startLabel] ++
    [Label endLabel]
```

**Example:**
```c
while (x > 0) {
    x = x - 1;
}
```

**IR:**
```
Label while_start_1
  Load "x"
  Push 0
  Gt
  JumpIfFalse while_end_1
  Load "x"
  Push 1
  Sub
  Store "x"
  Jump while_start_1
Label while_end_1
```

### Function Translation

```haskell
compileFunction :: FuncDef -> [Instruction]
compileFunction func =
  [Label (funcName func)] ++
  concatMap compileStatement (funcBody func)
```

**Example:**
```c
int add(int a, int b) {
    return a + b;
}
```

**IR:**
```
Label "add"
  Load "a"
  Load "b"
  Add
  Return
```

### Program Translation

```haskell
compileProgram :: Program -> [Instruction]
compileProgram (Program funcs) =
  -- Compile all functions
  concatMap compileFunction funcs ++
  -- Entry point: call main
  [Call "main" 0, Return]
```

## Label Generation

Labels must be unique to avoid conflicts:

```haskell
type LabelCounter = Int

genLabel :: String -> LabelCounter -> (String, LabelCounter)
genLabel prefix counter =
  (prefix ++ "_" ++ show counter, counter + 1)

-- Usage:
-- "if_1", "if_2", "while_start_1", "while_end_1", etc.
```

## IR Optimization (Future Work)

Potential optimizations:

1. **Constant Folding**: Evaluate constant expressions at compile time
2. **Dead Code Elimination**: Remove unreachable code after Return
3. **Peephole Optimization**: Replace instruction sequences with more efficient ones
4. **Register Allocation**: Convert to register-based IR for better performance

Currently, GLaDOS does **no optimization** - it generates naive, straightforward code.

## Complete Examples

### Example 1: Factorial (Recursive)

**Source:**
```c
int factorial(int n) {
    if (n <= 1) {
        return 1;
    }
    return n * factorial(n - 1);
}
```

**IR:**
```
Label "factorial"
  Load "n"
  Push 1
  Lte
  JumpIfFalse else_1
    Push 1
    Return
  Jump endif_1
Label else_1
Label endif_1
  Load "n"
  Load "n"
  Push 1
  Sub
  Call "factorial" 1
  Mul
  Return
```

### Example 2: Sum Loop (Iterative)

**Source:**
```c
int sum(int n) {
    int total = 0;
    int i = 1;
    while (i <= n) {
        total = total + i;
        i = i + 1;
    }
    return total;
}
```

**IR:**
```
Label "sum"
  Push 0
  Store "total"
  Push 1
  Store "i"
Label while_start_1
  Load "i"
  Load "n"
  Lte
  JumpIfFalse while_end_1
    Load "total"
    Load "i"
    Add
    Store "total"
    Load "i"
    Push 1
    Add
    Store "i"
    Jump while_start_1
Label while_end_1
  Load "total"
  Return
```

### Example 3: Boolean Logic

**Source:**
```c
bool isInRange(int x, int low, int high) {
    return (x >= low) && (x <= high);
}
```

**IR:**
```
Label "isInRange"
  Load "x"
  Load "low"
  Gte
  Load "x"
  Load "high"
  Lte
  And
  Return
```

## IR Properties

### Stack-Based Architecture

- **No registers**: All operations use the stack
- **Simple**: Easy to generate and execute
- **Compact**: Minimal instruction encoding
- **Portable**: Platform-independent

### Type Erasure

IR does **not** include type information:
- All values are either `IntVal` or `BoolVal`
- Type checking is complete before IR generation
- VM trusts the IR is well-typed

### Control Flow

- **Structured**: Uses labels and jumps
- **No goto**: Only conditional and unconditional jumps
- **Function calls**: Explicit `Call` and `Return`

## IR Verification

Although not currently implemented, IR can be verified for:

1. **Label validity**: All jump targets exist
2. **Stack balance**: Stack depth is consistent
3. **Type safety**: Operations receive correct value types
4. **Return coverage**: All functions return

## Performance Characteristics

### Code Size

- **Linear**: O(n) where n is AST size
- **Minimal overhead**: Simple 1:1 or 1:few AST-to-IR mapping
- **No optimization**: Code size could be reduced with optimizations

### Generation Time

- **Linear**: O(n) traversal of AST
- **Fast**: No complex analysis required
- **Single-pass**: Generate IR in one traversal

## IR as Compilation Target

The IR serves as:

1. **Abstraction layer**: Decouples front-end from VM
2. **Optimization target**: Future optimizations operate on IR
3. **Portable format**: Could target multiple back-ends
4. **Debug format**: Can be pretty-printed for debugging

## Testing IR Generation

### Example Tests

```haskell
-- Test: Simple expression
testSimpleExpr :: Spec
testSimpleExpr = it "compiles 5 + 3" $ do
  let ast = BinOp Add (Lit (IntLit 5)) (Lit (IntLit 3))
  compileExpr ast `shouldBe` [Push (IntVal 5), Push (IntVal 3), Add]

-- Test: Variable operations
testVariable :: Spec
testVariable = it "compiles assignment" $ do
  let ast = Assign "x" (Lit (IntLit 42))
  compileStatement ast `shouldBe` [Push (IntVal 42), Store "x"]

-- Test: Control flow
testIf :: Spec
testIf = it "compiles if statement" $ do
  let ast = If (Lit (BoolLit True)) [Return (Lit (IntLit 1))] Nothing
  length (compileStatement ast) `shouldBe` 5  -- cond + jump + body + label
```

### Running Tests

```bash
stack test --test-arguments="--match Compiler"
```

## Next Steps

After IR generation, the bytecode is executed by:

- [VM Execution](./vm-execution.md): Stack machine executing IR instructions

See also:
- [Compilation Overview](./overview.md): High-level compilation pipeline
- [Type Checking](./type-checking.md): Type safety before IR generation
