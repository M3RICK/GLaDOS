# Type System

GLaDOS features a **static, strong type system** that ensures type safety at compile time. This document provides a comprehensive reference for the type system.

## Primitive Types

GLaDOS supports four primitive types:

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
- No implicit conversion to/from bool or float

### Float Type (`float`)

- **Size**: 64-bit double-precision floating-point
- **Range**: Approximately ±1.7 × 10^308 (IEEE 754 double precision)
- **Precision**: ~15-17 significant decimal digits
- **Haskell Representation**: `Double`
- **Literals**: Decimal floating-point literals (e.g., `3.14`, `-0.5`, `1.0`, `2.5e10`)

**Examples:**
```c
float pi = 3.14159;
float negative = -2.5;
float small = 0.001;
float scientific = 1.5e-10;
```

**Operations:**
- Arithmetic: `+`, `-`, `*`, `/`
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
- No implicit conversion to/from int or bool

**Important Notes:**
- Float literals must include a decimal point (e.g., `1.0`, not `1`)
- Division by zero results in runtime error (like integers)
- Floating-point arithmetic follows IEEE 754 standard
- No automatic conversion between `int` and `float`

**Type Safety with Floats:**
```c
float x = 3.14;
float y = 2.0;
float z = x + y;

int a = 5;
float b = a;
float c = 3.14 + 5;
```

### Boolean Type (`bool`)

- **Values**: `true` or `false`
- **Haskell Representation**: `Bool`
- **Usage**: Conditions, logical operations, comparison results

**Examples:**
```c
bool flag = true;
bool isReady = false;
bool result = 5 > 3;
```

**Operations:**
- Logical: `&&` (AND), `||` (OR)
- Comparison: `==`, `!=`
- No implicit conversion to/from int or float

### Void Type (`void`)

- **Purpose**: Indicates functions that do not return a value
- **Usage**: Only valid as a function return type
- **Haskell Representation**: `TypeVoid`

**Examples:**
```c
void printMessage() {

}

void doSomething(int x) {
    if (x > 0) {
        return;
    }
}
```

**Restrictions:**
```c
void x;

void result = someFunction();

void helper(int a, int b) {

}
```

**Important Notes:**
- Void functions do not need explicit return statements
- Can use `return;` (without value) for early exit
- Cannot be used as variable type
- Cannot be used as parameter type
- Type checker does not require return path analysis for void functions

## Type Safety

### Static Type Checking

All types are checked at **compile time** before the program runs. This catches type errors early:

```c
int x = true;

float y = 5;

int x = 42;
float y = 3.14;
bool flag = true;
```

### Strong Typing

GLaDOS does **not** allow implicit type conversions:

```c
int x = 5 + true;

float y = 3.14 + 5;

if (5) {
    return 1;
}

int z = 3.14;

if (5 > 0) {
    return 1;
}

float a = 3.14;
float b = a + 2.0;
```

### Type Inference

GLaDOS supports **type inference** using the `var` keyword. Variables declared with `var` must be initialized, and their type is inferred from the initialization expression:

```c
var x = 42;        // Inferred as int
var y = 3.14;      // Inferred as float
var z = true;      // Inferred as bool

int x = 42;        // Explicit type declaration (also valid)
```

**Requirements for `var`:**
- Must be initialized at declaration (required)
- Type is inferred from the initialization expression
- Provides compile-time type safety

**Note:** The compiler uses an internal `TypeInfer` type during type checking for intermediate expressions to enable the `var` keyword functionality.

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
int square(int x) {
    return x * x;
}

int add(int a, int b) {
    return a + b;
}

float multiply(float x, float y) {
    return x * y;
}

bool isGreater(int a, int b) {
    return a > b;
}

int getFortyTwo() {
    return 42;
}

void printNumber(int x) {

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
    int x = add(5);

    int y = add(true, 5);

    int z = add(5, 3);
    return z;
}
```

## Type Rules

### Variable Declaration

Variables must be declared with a type before use:

```c
int x;
x = 42;
int y = 10;

float pi = 3.14;
bool flag = true;
```

### Variable Initialization

GLaDOS tracks whether variables are initialized before use:

```c
int x;
return x;

int x = 42;
return x;

int y;
y = 10;
return y;
```

### Assignment Type Checking

Assignments must match the variable's declared type:

```c
int x = 5;
x = 10;
x = true;
```

### Expression Type Rules

Every expression has a type determined by its components:

#### Arithmetic Expressions

**Rule**: Both operands must be the same numeric type (`int` or `float`), result is same type

```c
int a = 5 + 3;
int b = 10 - 2;
int c = 4 * 5;
int d = 20 / 4;

float e = 3.14 + 2.86;
float f = 10.5 - 3.2;
float g = 2.5 * 4.0;
float h = 9.0 / 3.0;

int i = 5 + true;
int j = 5 + 3.14;
float k = 3.14 + 5;
```

#### Comparison Expressions

**Rule**: Both operands must be same type, result is `bool`

```c
bool a = 5 == 5;
bool b = 5 != 3;
bool c = 5 < 10;

bool d = 3.14 == 3.14;
bool e = 2.5 < 3.7;
bool f = 1.0 >= 0.5;

bool g = true == false;

bool h = 5 == true;
bool i = 3.14 == 5;
bool j = 5 < 3.14;
```

#### Logical Expressions

**Rule**: Both operands must be `bool`, result is `bool`

```c
bool a = true && false;
bool b = true || false;
bool c = (5 > 3) && (10 < 20);

bool d = 5 && 10;
bool e = true && 5;
```

### Control Flow Type Rules

#### If Statement Condition

**Rule**: Condition must be type `bool`

```c
if (5 > 3) {
    return 1;
}

bool flag = true;
if (flag) {
    return 1;
}

if (5) {
    return 1;
}
```

#### While Loop Condition

**Rule**: Condition must be type `bool`

```c
while (x > 0) {
    x = x - 1;
}

while (x) {
    x = x - 1;
}
```

### Return Statement Type Rules

**Rule**: Return expression type must match function return type

```c
int getValue() {
    return 42;
}

float getPi() {
    return 3.14159;
}

bool isReady() {
    return true;
}

void doSomething() {

}

int getBad() {
    return true;
}

float getWrong() {
    return 5;
}

void voidFunc() {
    return 42;
}
```

### All Paths Must Return

**Rule**: Every non-void function code path must return a value of the correct type

```c
int bad(int x) {
    if (x > 0) {
        return 1;
    }
}

int good(int x) {
    if (x > 0) {
        return 1;
    } else {
        return 0;
    }
}

int alsoGood(int x) {
    if (x > 0) {
        return 1;
    }
    return 0;
}

void printValue(int x) {
    if (x > 0) {
        return;
    }
}
```

**Note:** The return path analysis only applies to non-void functions. Void functions are not required to have return statements on all paths.

## Type Error Messages

The type checker provides detailed error messages:

```
Type error at line 5: expected int but got bool
Type error at line 8: expected float but got int
Return type mismatch at line 10: expected int but got bool
Return type mismatch at line 15: expected void but got int
Type error in arithmetic at line 7: expected int but got bool
Type error in arithmetic at line 12: cannot mix int and float
Function 'add' expects 2 arguments but got 1 at line 15
Type mismatch in function argument at line 12: expected int but got bool
Type mismatch in function argument at line 18: expected float but got int
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
  = TypeInt
  | TypeFloat
  | TypeBool
  | TypeVoid
  | TypeInfer
  deriving (Show, Eq)
```

**Notes:**
- `TypeInfer` is used internally by the compiler during type checking for intermediate expressions
- User code cannot explicitly use the `TypeInfer` type
- All variable declarations require explicit type annotations

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

### No User-Level Type Inference

Variables must have explicit type declarations. This is by design for simplicity and clarity.

**Note:** The compiler internally uses a `TypeInfer` type during type checking for intermediate expressions, but this is not exposed to user code. All variable and parameter declarations require explicit type annotations.

### No Implicit Conversions

No automatic conversions between types (e.g., int to bool, int to float, or vice versa). This prevents common bugs.

**Examples of rejected implicit conversions:**
```c
int x = 5;
float y = x;

float a = 3.14;
int b = a;

bool flag = 1;

int count = true;
```

All type conversions must be explicit (note: GLaDOS currently does not provide explicit conversion functions, so values must be literals of the correct type).

### No User-Defined Types

GLaDOS does not support:
- Structs or records
- Enums
- Type aliases
- Generic/parametric types

This keeps the language simple and focused.

### Void Return Type

Functions can have `void` return type when they don't return a value:

```c
void helper(int x) {

}

void earlyExit(bool condition) {
    if (condition) {
        return;
    }
}
```

**Important:**
- Void can only be used as a function return type
- Void functions don't need explicit return statements
- Can use `return;` (without expression) for early exit
- Cannot declare void variables or parameters
- Type checker doesn't enforce return path analysis for void functions

### No Null or Optional Types

There is no concept of null, undefined, or optional values. All variables must be initialized.

## Type Safety Guarantees

GLaDOS's type system guarantees:

1. **Type Soundness**: Well-typed programs cannot have type errors at runtime
2. **Initialization Safety**: Variables cannot be read before being initialized
3. **Return Safety**: Functions always return a value of the correct type
4. **Operation Safety**: Operators are only applied to compatible types

These guarantees eliminate entire classes of bugs at compile time.
