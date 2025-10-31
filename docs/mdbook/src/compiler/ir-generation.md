# IR Generation

This page details how GLaDOS translates the type-checked AST into Intermediate Representation (IR), a WebAssembly-like stack-based bytecode suitable for VM execution.

## Overview

IR generation is implemented in `src/Compiler/` modules and produces instructions defined in `src/IR/Types.hs`. The IR uses:

- **Stack-based execution**: All operations work with an operand stack
- **Indexed locals**: Variables accessed by integer index, not name
- **Type-specific instructions**: Separate operations for int, float, and bool
- **Offset-based control flow**: Jumps use relative instruction offsets
- **Function indexing**: Functions called by index into function table

## IR Program Structure

An IR program consists of multiple compiled functions:

```haskell
data IRProgram = IRProgram
  { functions :: [CompiledFunction]
  , mainIndex :: Int
  }
```

Each function is compiled separately:

```haskell
data CompiledFunction = CompiledFunction
  { funcName :: String
  , paramCount :: Int
  , localVarCount :: Int
  , code :: [Instruction]
  }
```

**Local Variable Indexing:**

Locals are numbered starting from 0:
- Parameters occupy indices `0` to `paramCount - 1`
- Local variables occupy indices `paramCount` to `localVarCount - 1`

Example:
```c
int add(int a, int b) {
    int result = a + b;
    return result;
}
```

Local indices:
- `a` = local 0 (parameter)
- `b` = local 1 (parameter)
- `result` = local 2 (local variable)

## IR Instruction Set

Complete instruction set from `src/IR/Types.hs`:

### Stack Operations

**`PushInt Int`**
- Pushes an integer literal onto the stack
- Example: `PushInt 42`

**`PushBool Bool`**
- Pushes a boolean literal onto the stack
- Example: `PushBool True`

**`PushFloat Double`**
- Pushes a float literal onto the stack
- Example: `PushFloat 3.14`

**`Pop`**
- Pops and discards the top value from the stack
- Used for expression statements

### Local Variable Operations

**`GetLocal Int`**
- Pushes the value of local variable at given index onto the stack
- Example: `GetLocal 0` loads first parameter/local

**`SetLocal Int`**
- Pops a value from stack and stores it in local variable at given index
- Example: `SetLocal 2` stores to third local

### Integer Arithmetic

**`AddInt`**
- Pops `b`, pops `a`, pushes `a + b` (integers)

**`SubInt`**
- Pops `b`, pops `a`, pushes `a - b` (integers)

**`MulInt`**
- Pops `b`, pops `a`, pushes `a * b` (integers)

**`DivInt`**
- Pops `b`, pops `a`, pushes `a / b` (integers)
- Runtime error if `b` is 0

### Float Arithmetic

**`AddFloat`**
- Pops `b`, pops `a`, pushes `a + b` (floats)

**`SubFloat`**
- Pops `b`, pops `a`, pushes `a - b` (floats)

**`MulFloat`**
- Pops `b`, pops `a`, pushes `a * b` (floats)

**`DivFloat`**
- Pops `b`, pops `a`, pushes `a / b` (floats)
- Runtime error if `b` is 0.0

### Unary Operations

**`NegInt`**
- Pops integer `a`, pushes `-a`

**`NegFloat`**
- Pops float `a`, pushes `-a`

**`NotBool`**
- Pops boolean `a`, pushes `!a`

### Integer Comparison

**`EqInt`**
- Pops `b`, pops `a`, pushes boolean `a == b`

**`NeqInt`**
- Pops `b`, pops `a`, pushes boolean `a != b`

**`LtInt`**
- Pops `b`, pops `a`, pushes boolean `a < b`

**`GtInt`**
- Pops `b`, pops `a`, pushes boolean `a > b`

**`LeInt`**
- Pops `b`, pops `a`, pushes boolean `a <= b`

**`GeInt`**
- Pops `b`, pops `a`, pushes boolean `a >= b`

### Float Comparison

**`EqFloat`**
- Pops `b`, pops `a`, pushes boolean `a == b`

**`NeqFloat`**
- Pops `b`, pops `a`, pushes boolean `a != b`

**`LtFloat`**
- Pops `b`, pops `a`, pushes boolean `a < b`

**`GtFloat`**
- Pops `b`, pops `a`, pushes boolean `a > b`

**`LeFloat`**
- Pops `b`, pops `a`, pushes boolean `a <= b`

**`GeFloat`**
- Pops `b`, pops `a`, pushes boolean `a >= b`

### Logical Operations

**`AndBool`**
- Pops `b`, pops `a`, pushes boolean `a && b`
- Note: Both operands are always evaluated (not short-circuit in IR)

**`OrBool`**
- Pops `b`, pops `a`, pushes boolean `a || b`
- Note: Both operands are always evaluated (not short-circuit in IR)

### Control Flow

**`Jump Int`**
- Unconditional jump by relative offset
- Example: `Jump 5` jumps forward 5 instructions
- Example: `Jump -3` jumps backward 3 instructions

**`JumpIfFalse Int`**
- Pops top of stack (must be boolean)
- If false, jumps by relative offset
- If true, continues to next instruction

**`Call Int`**
- Calls function at given index in function table
- Function pops its parameters from stack
- Function pushes return value onto stack

**`Return`**
- Returns from current function
- Top of stack is return value (for non-void functions)
- Void functions have no value on stack

**`Halt`**
- Ends program execution
- Used after main function returns

## Type-Specific Compilation

The compiler selects instructions based on operand types:

### Integer Expression
```c
int x = 5 + 3;
```

Compiles to:
```
PushInt 5
PushInt 3
AddInt
SetLocal 0
```

### Float Expression
```c
float y = 2.5 * 4.0;
```

Compiles to:
```
PushFloat 2.5
PushFloat 4.0
MulFloat
SetLocal 0
```

### Boolean Expression
```c
bool flag = x > 10;
```

Compiles to:
```
GetLocal 0
PushInt 10
GtInt
SetLocal 1
```

## AST to IR Translation

### Expression Translation

**Integer Literal:**
```c
42
```
→ `PushInt 42`

**Float Literal:**
```c
3.14
```
→ `PushFloat 3.14`

**Boolean Literal:**
```c
true
```
→ `PushBool True`

**Variable Reference:**
```c
x
```
→ `GetLocal <index>`

**Binary Operation:**
```c
a + b
```
→
```
GetLocal <a_index>
GetLocal <b_index>
AddInt
```

**Unary Operation:**
```c
-x
```
→
```
GetLocal <x_index>
NegInt
```

**Function Call:**
```c
add(5, 3)
```
→
```
PushInt 5
PushInt 3
Call <add_func_index>
```

### Statement Translation

**Variable Declaration:**
```c
int x = 10;
```
→
```
PushInt 10
SetLocal 0
```

**Assignment:**
```c
x = 20;
```
→
```
PushInt 20
SetLocal 0
```

**Return Statement:**
```c
return x + 1;
```
→
```
GetLocal 0
PushInt 1
AddInt
Return
```

**Expression Statement:**
```c
factorial(5);
```
→
```
PushInt 5
Call <factorial_index>
Pop
```

### Control Flow Translation

#### If Statement

**Source:**
```c
if (x > 0) {
    return x;
} else {
    return 0;
}
```

**IR:**
```
GetLocal 0
PushInt 0
GtInt
JumpIfFalse 3
GetLocal 0
Return
Jump 2
PushInt 0
Return
```

**Explanation:**
1. Evaluate condition `x > 0`
2. `JumpIfFalse 3`: if false, jump forward 3 instructions (skip then branch)
3. Then branch: get x and return
4. `Jump 2`: skip else branch
5. Else branch: push 0 and return

#### While Loop

**Source:**
```c
int i = 0;
while (i < 10) {
    i = i + 1;
}
```

**IR:**
```
PushInt 0
SetLocal 0
GetLocal 0
PushInt 10
LtInt
JumpIfFalse 6
GetLocal 0
PushInt 1
AddInt
SetLocal 0
Jump -9
```

**Explanation:**
1. Initialize `i = 0`
2. Loop condition: `i < 10`
3. `JumpIfFalse 6`: exit loop if condition false
4. Loop body: `i = i + 1`
5. `Jump -9`: jump back to condition

#### For Loop

**Source:**
```c
for (int i = 0; i < 10; i = i + 1) {
    sum = sum + i;
}
```

**IR:**
```
PushInt 0
SetLocal 1
GetLocal 1
PushInt 10
LtInt
JumpIfFalse 10
GetLocal 0
GetLocal 1
AddInt
SetLocal 0
GetLocal 1
PushInt 1
AddInt
SetLocal 1
Jump -13
```

## Function Translation

### Simple Function

**Source:**
```c
int double(int x) {
    return x * 2;
}
```

**CompiledFunction:**
```haskell
CompiledFunction {
  funcName = "double",
  paramCount = 1,
  localVarCount = 1,
  code = [
    GetLocal 0,
    PushInt 2,
    MulInt,
    Return
  ]
}
```

### Function with Locals

**Source:**
```c
int add(int a, int b) {
    int result = a + b;
    return result;
}
```

**CompiledFunction:**
```haskell
CompiledFunction {
  funcName = "add",
  paramCount = 2,
  localVarCount = 3,
  code = [
    GetLocal 0,
    GetLocal 1,
    AddInt,
    SetLocal 2,
    GetLocal 2,
    Return
  ]
}
```

**Local indices:**
- Local 0: parameter `a`
- Local 1: parameter `b`
- Local 2: local variable `result`

### Void Function

**Source:**
```c
void printValue(int x) {
    if (x > 0) {
        return;
    }
}
```

**CompiledFunction:**
```haskell
CompiledFunction {
  funcName = "printValue",
  paramCount = 1,
  localVarCount = 1,
  code = [
    GetLocal 0,
    PushInt 0,
    GtInt,
    JumpIfFalse 1,
    Return,
    Return
  ]
}
```

## Program Translation

### Function Table

Functions are indexed for calling:

```c
int helper(int x) {
    return x * 2;
}

int main() {
    return helper(21);
}
```

**Function Table:**
- Index 0: `helper`
- Index 1: `main`

**main function IR:**
```
PushInt 21
Call 0
Return
```

### Main Index

The `IRProgram` stores the index of the main function:

```haskell
IRProgram {
  functions = [helperCompiled, mainCompiled],
  mainIndex = 1
}
```

## Offset Calculation

Jump offsets are calculated relative to the current instruction.

### Forward Jump

**Code:**
```
0: GetLocal 0
1: JumpIfFalse 3
2: PushInt 1
3: Return
4: PushInt 0
5: Return
```

At instruction 1, `JumpIfFalse 3` jumps to instruction 4 (1 + 3).

### Backward Jump

**Code:**
```
0: GetLocal 0
1: PushInt 10
2: LtInt
3: JumpIfFalse 4
4: GetLocal 0
5: PushInt 1
6: AddInt
7: SetLocal 0
8: Jump -8
9: ...
```

At instruction 8, `Jump -8` jumps to instruction 0 (8 - 8).

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

**CompiledFunction:**
```haskell
CompiledFunction {
  funcName = "factorial",
  paramCount = 1,
  localVarCount = 1,
  code = [
    GetLocal 0,
    PushInt 1,
    LeInt,
    JumpIfFalse 2,
    PushInt 1,
    Return,
    GetLocal 0,
    GetLocal 0,
    PushInt 1,
    SubInt,
    Call 0,
    MulInt,
    Return
  ]
}
```

**Execution trace for `factorial(3)`:**
```
Stack: []
GetLocal 0          → Stack: [3]
PushInt 1           → Stack: [3, 1]
LeInt               → Stack: [False]
JumpIfFalse 2       → Stack: [], PC = 6
GetLocal 0          → Stack: [3]
GetLocal 0          → Stack: [3, 3]
PushInt 1           → Stack: [3, 3, 1]
SubInt              → Stack: [3, 2]
Call 0              → Stack: [3, 2] (recursive call)
...eventually returns with Stack: [3, 2]
MulInt              → Stack: [6]
Return              → Returns 6
```

### Example 2: Sum Loop (Iterative)

**Source:**
```c
int sum(int n) {
    int total = 0;
    int i = 0;
    while (i < n) {
        total = total + i;
        i = i + 1;
    }
    return total;
}
```

**CompiledFunction:**
```haskell
CompiledFunction {
  funcName = "sum",
  paramCount = 1,
  localVarCount = 3,
  code = [
    PushInt 0,
    SetLocal 1,
    PushInt 0,
    SetLocal 2,
    GetLocal 2,
    GetLocal 0,
    LtInt,
    JumpIfFalse 10,
    GetLocal 1,
    GetLocal 2,
    AddInt,
    SetLocal 1,
    GetLocal 2,
    PushInt 1,
    AddInt,
    SetLocal 2,
    Jump -12,
    GetLocal 1,
    Return
  ]
}
```

**Local indices:**
- Local 0: parameter `n`
- Local 1: local variable `total`
- Local 2: local variable `i`

### Example 3: Float Arithmetic

**Source:**
```c
float circleArea(float radius) {
    float pi = 3.14159;
    return pi * radius * radius;
}
```

**CompiledFunction:**
```haskell
CompiledFunction {
  funcName = "circleArea",
  paramCount = 1,
  localVarCount = 2,
  code = [
    PushFloat 3.14159,
    SetLocal 1,
    GetLocal 1,
    GetLocal 0,
    MulFloat,
    GetLocal 0,
    MulFloat,
    Return
  ]
}
```

**Note:** All float operations use type-specific instructions.

### Example 4: Boolean Logic

**Source:**
```c
bool isInRange(int x, int low, int high) {
    return (x >= low) && (x <= high);
}
```

**CompiledFunction:**
```haskell
CompiledFunction {
  funcName = "isInRange",
  paramCount = 3,
  localVarCount = 3,
  code = [
    GetLocal 0,
    GetLocal 1,
    GeInt,
    GetLocal 0,
    GetLocal 2,
    LeInt,
    AndBool,
    Return
  ]
}
```

## Compilation Environment

### Variable Table

The compiler maintains a `VarTable` mapping variable names to local indices:

```haskell
type VarTable = Map String Int
```

**Example for function `add(int a, int b)`:**
```haskell
varTable = Map.fromList [("a", 0), ("b", 1), ("result", 2)]
```

### Function Table

The compiler maintains a `FuncTable` mapping function names to indices:

```haskell
type FuncTable = Map String Int
```

**Example:**
```haskell
funcTable = Map.fromList [("factorial", 0), ("main", 1), ("helper", 2)]
```

## IR Properties

### Stack-Based Architecture

- All operations consume operands from stack and push results
- No register allocation needed
- Simple to generate, simple to interpret

### Type-Specific Operations

- Type information encoded in instructions (AddInt vs AddFloat)
- No runtime type checking needed
- VM execution is fast and predictable

### Indexed Locals

- Variables accessed by integer index
- No runtime name lookup
- Fast and efficient

### Offset-Based Control Flow

- No label resolution needed
- Direct jumps with known offsets
- Compact representation

## Name Resolution

During compilation, all names are resolved to indices:

1. **Function names** → Function table index
2. **Variable names** → Local variable index
3. **Jump labels** → Relative instruction offset

This happens during IR generation, so the VM never sees string names.

## Performance Characteristics

### Code Size

- Compact representation
- Instructions are small (most are 1-2 bytes when serialized)
- Local indices fit in single bytes for most functions

### Generation Time

- Single-pass compilation after type checking
- Linear time in AST size
- No complex optimizations (yet)

## IR as Compilation Target

The IR design is inspired by WebAssembly:

- Indexed locals (like WebAssembly)
- Type-specific instructions (like WebAssembly)
- Stack-based execution (like WebAssembly)
- Function indexing (like WebAssembly)

This makes it:
- Simple to generate
- Simple to interpret
- Simple to serialize
- Fast to execute

## Next Steps

After IR generation:
1. IR is passed to bytecode serializer (see Bytecode Generation)
2. Bytecode is executed by VM (see VM Execution)
