# Type System

GLaDOS features a **static, strong type system** that ensures type safety at compile time. This document provides a comprehensive reference for the type system.

## Primitive Types

GLaDOS supports two primitive types:

### Integer Type (`int`)

- **Size**: 64-bit signed integer
- **Range**: -9,223,372,036,854,775,808 to 9,223,372,036,854,775,807
- **Haskell Representation**: `Int64`
- **Literals**: Decimal integer literals (e.g., `0`, `42`, `-100`, `999999`)

**Examples:**
```c
int x = 42;
int negative = -10;
int zero = 0;
int large = 1000000;
```

**Operations:**
- Arithmetic: `+`, `-`, `*`, `/`
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
- No implicit conversion to/from bool

### Boolean Type (`bool`)

- **Values**: `true` or `false`
- **Haskell Representation**: `Bool`
- **Usage**: Conditions, logical operations, comparison results

**Examples:**
```c
bool flag = true;
bool isReady = false;
bool result = 5 > 3;  // true
```

**Operations:**
- Logical: `&&` (AND), `||` (OR)
- Comparison: `==`, `!=`
- No implicit conversion to/from int

## Type Safety

### Static Type Checking

All types are checked at **compile time** before the program runs. This catches type errors early:

```c
// ✗ COMPILE ERROR: Type mismatch
int x = true;

// ✓ OK
int x = 42;
bool flag = true;
```

### Strong Typing

GLaDOS does **not** allow implicit type conversions:

```c
// ✗ COMPILE ERROR: Cannot mix types
int x = 5 + true;

// ✗ COMPILE ERROR: bool expected, got int
if (5) {
    return 1;
}

// ✓ OK: Explicit comparison
if (5 > 0) {
    return 1;
}
```

### Type Inference

Variable types must be **explicitly declared**. GLaDOS does not infer types:

```c
// ✗ ERROR: Type must be specified
var x = 42;

// ✓ OK: Type explicitly declared
int x = 42;
```

## Function Types

Functions have a type signature consisting of:
- **Return type**: Type of value returned
- **Parameter types**: Types of all parameters (in order)

### Function Signature Syntax

```c
<return_type> <function_name>(<param_type> <param_name>, ...)
```

**Examples:**
```c
// Type: int -> int
int square(int x) {
    return x * x;
}

// Type: (int, int) -> int
int add(int a, int b) {
    return a + b;
}

// Type: (int, int) -> bool
bool isGreater(int a, int b) {
    return a > b;
}

// Type: () -> int (no parameters)
int getFortyTwo() {
    return 42;
}
```

### Type Checking Rules

1. **Argument Count**: Must match parameter count exactly
2. **Argument Types**: Must match parameter types exactly (in order)
3. **Return Type**: All return statements must match declared return type

```c
int add(int a, int b) {
    return a + b;
}

int main() {
    // ✗ ERROR: Expected 2 arguments, got 1
    int x = add(5);

    // ✗ ERROR: Expected int, got bool
    int y = add(true, 5);

    // ✓ OK
    int z = add(5, 3);
    return z;
}
```

## Type Rules

### Variable Declaration

Variables must be declared with a type before use:

```c
int x;        // Declaration
x = 42;       // Assignment
int y = 10;   // Declaration with initialization
```

### Variable Initialization

GLaDOS tracks whether variables are initialized before use:

```c
// ✗ ERROR: Variable used before initialization
int x;
return x;

// ✓ OK: Variable initialized
int x = 42;
return x;

// ✓ OK: Variable assigned before use
int y;
y = 10;
return y;
```

### Assignment Type Checking

Assignments must match the variable's declared type:

```c
int x = 5;
x = 10;      // ✓ OK: int = int
x = true;    // ✗ ERROR: int = bool
```

### Expression Type Rules

Every expression has a type determined by its components:

#### Arithmetic Expressions

**Rule**: Both operands must be `int`, result is `int`

```c
int a = 5 + 3;      // ✓ int + int = int
int b = 10 - 2;     // ✓ int - int = int
int c = 4 * 5;      // ✓ int * int = int
int d = 20 / 4;     // ✓ int / int = int

int e = 5 + true;   // ✗ ERROR: int + bool
```

#### Comparison Expressions

**Rule**: Both operands must be same type, result is `bool`

```c
bool a = 5 == 5;     // ✓ int == int = bool
bool b = 5 != 3;     // ✓ int != int = bool
bool c = 5 < 10;     // ✓ int < int = bool
bool d = true == false;  // ✓ bool == bool = bool

bool e = 5 == true;  // ✗ ERROR: Cannot compare int and bool
```

#### Logical Expressions

**Rule**: Both operands must be `bool`, result is `bool`

```c
bool a = true && false;        // ✓ bool && bool = bool
bool b = true || false;        // ✓ bool || bool = bool
bool c = (5 > 3) && (10 < 20); // ✓ bool && bool = bool

bool d = 5 && 10;              // ✗ ERROR: int && int
bool e = true && 5;            // ✗ ERROR: bool && int
```

### Control Flow Type Rules

#### If Statement Condition

**Rule**: Condition must be type `bool`

```c
// ✓ OK: Condition is bool
if (5 > 3) {
    return 1;
}

// ✓ OK: Variable is bool
bool flag = true;
if (flag) {
    return 1;
}

// ✗ ERROR: Condition is int, not bool
if (5) {
    return 1;
}
```

#### While Loop Condition

**Rule**: Condition must be type `bool`

```c
// ✓ OK
while (x > 0) {
    x = x - 1;
}

// ✗ ERROR: int where bool expected
while (x) {
    x = x - 1;
}
```

### Return Statement Type Rules

**Rule**: Return expression type must match function return type

```c
// ✓ OK: Returns int
int getValue() {
    return 42;
}

// ✗ ERROR: Returns bool from int function
int getBad() {
    return true;
}

// ✓ OK: Returns bool
bool isReady() {
    return true;
}
```

### All Paths Must Return

**Rule**: Every code path must return a value of the correct type

```c
// ✗ ERROR: Missing return in else branch
int bad(int x) {
    if (x > 0) {
        return 1;
    }
    // No return here!
}

// ✓ OK: All paths return
int good(int x) {
    if (x > 0) {
        return 1;
    } else {
        return 0;
    }
}

// ✓ OK: Return after if
int alsoGood(int x) {
    if (x > 0) {
        return 1;
    }
    return 0;
}
```

## Type Error Messages

The type checker provides detailed error messages:

```
Type error at line 5: expected int but got bool
Return type mismatch at line 10: expected int but got bool
Type error in arithmetic at line 7: expected int but got bool
Function 'add' expects 2 arguments but got 1 at line 15
Type mismatch in function argument at line 12: expected int but got bool
```

Each message includes:
- **Error category** (Type error, Return type mismatch, etc.)
- **Source location** (line number)
- **Expected type** vs **Actual type**

## Type System Implementation

### Internal Representation

Types are represented in Haskell as:

```haskell
data Type
  = TInt      -- Integer type
  | TBool     -- Boolean type
  deriving (Eq, Show)
```

### Type Checking Algorithm

The type checker performs:

1. **Environment Building**: Collect all function signatures
2. **Function Validation**: Check each function body
3. **Expression Type Inference**: Determine type of each expression
4. **Type Constraint Verification**: Ensure all operations are type-safe
5. **Initialization Tracking**: Verify variables are initialized before use
6. **Return Path Analysis**: Ensure all paths return correct type

### Type Environment

The type checker maintains:
- **Global Environment**: Function signatures
- **Local Environment**: Local variable types and initialization status
- **Expected Return Type**: Current function's return type

## Limitations and Design Choices

### No Type Inference

Variables must have explicit type declarations. This is by design for simplicity and clarity.

### No Implicit Conversions

No automatic conversions between types (e.g., int to bool or vice versa). This prevents common bugs.

### No User-Defined Types

GLaDOS does not support:
- Structs or records
- Enums
- Type aliases
- Generic/parametric types

This keeps the language simple and focused.

### No Void Functions

All functions must return a value. There is no `void` return type.

### No Null or Optional Types

There is no concept of null, undefined, or optional values. All variables must be initialized.

## Type Safety Guarantees

GLaDOS's type system guarantees:

1. **Type Soundness**: Well-typed programs cannot have type errors at runtime
2. **Initialization Safety**: Variables cannot be read before being initialized
3. **Return Safety**: Functions always return a value of the correct type
4. **Operation Safety**: Operators are only applied to compatible types

These guarantees eliminate entire classes of bugs at compile time.
