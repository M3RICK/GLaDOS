# Error Messages

> **TODO**: Update warnings to most recent polished warnings.

GLaDOS provides detailed error messages to help you debug your programs. This guide explains common errors and how to fix them.

## Compile-Time Errors

All errors below are caught during compilation (before your program runs).

### Type Errors

#### Type Mismatch in Assignment

```c
int x = true;
```

**Error:**
```
Type error at line 1: expected int but got bool
```

**Fix:** Use matching types:
```c
int x = 42;
bool flag = true;
```

#### Type Mismatch in Return

```c
int getValue() {
    return true;  // Returning bool from int function
}
```

**Error:**
```
Return type mismatch at line 2: expected int but got bool
```

**Fix:**
```c
int getValue() {
    return 42;
}
```

#### Type Mismatch in Operations

```c
int x = 5;
bool y = true;
int z = x + y;
```

**Error:**
```
Type error in arithmetic at line 3: expected int but got bool
```

**Fix:** Don't mix types in arithmetic:
```c
int x = 5;
int y = 10;
int z = x + y;  // Both operands are int
```

#### Type Mismatch in Conditionals

```c
int x = 5;
while (x) {  // int where bool expected
    x = x - 1;
}
```

**Error:**
```
Type error at line 2: expected bool but got int
```

**Fix:** Use a comparison:
```c
while (x > 0) {
    x = x - 1;
}
```

### Function Errors

#### Undefined Function

```c
int main() {
    return multiply(5, 3);  // multiply not defined
}
```

**Error:**
```
Undefined function 'multiply' at line 2
```

**Fix:** Define the function before calling it:
```c
int multiply(int a, int b) {
    return a * b;
}

int main() {
    return multiply(5, 3);
}
```

#### Wrong Argument Count

```c
int add(int a, int b) {
    return a + b;
}

int main() {
    return add(5);  // Missing one argument
}
```

**Error:**
```
Function 'add' expects 2 arguments but got 1 at line 6
```

**Fix:** Provide all required arguments:
```c
return add(5, 3);
```

#### Wrong Argument Types

```c
int add(int a, int b) {
    return a + b;
}

int main() {
    bool x = true;
    return add(x, 5);  // bool passed where int expected
}
```

**Error:**
```
Type mismatch in function argument at line 7: expected int but got bool
```

**Fix:** Use correct types:
```c
int x = 10;
return add(x, 5);
```

### Variable Errors

#### Undefined Variable

```c
int main() {
    return x;  // x not defined
}
```

**Error:**
```
Undefined variable 'x' at line 2
```

**Fix:** Declare the variable first:
```c
int main() {
    int x = 42;
    return x;
}
```

#### Uninitialized Variable

```c
int main() {
    int x;
    return x;  // x declared but not initialized
}
```

**Error:**
```
Variable 'x' used before initialization at line 3
```

**Fix:** Initialize before use:
```c
int main() {
    int x = 42;
    return x;
}
```

Or assign a value:
```c
int main() {
    int x;
    x = 42;
    return x;
}
```

#### Variable Used Before Declaration

```c
int main() {
    int y = x + 1;  // x used before declaration
    int x = 5;
    return y;
}
```

**Error:**
```
Undefined variable 'x' at line 2
```

**Fix:** Declare variables before using them:
```c
int main() {
    int x = 5;
    int y = x + 1;
    return y;
}
```

### Return Statement Errors

#### Missing Return Statement

```c
int getValue(int x) {
    if (x > 0) {
        return 1;
    }
    // Missing return for x <= 0
}
```

**Error:**
```
Function 'getValue' must return a value on all code paths
```

**Fix:** Add an else branch with return:
```c
int getValue(int x) {
    if (x > 0) {
        return 1;
    } else {
        return 0;
    }
}
```

Or a return after the if:
```c
int getValue(int x) {
    if (x > 0) {
        return 1;
    }
    return 0;
}
```

#### Empty Function Body

```c
int main() {
}
```

**Error:**
```
Function 'main' must return a value on all code paths
```

**Fix:** Add a return statement:
```c
int main() {
    return 0;
}
```

### Division Errors

#### Division by Zero (Literal)

```c
int main() {
    int x = 10 / 0;
    return x;
}
```

**Error:**
```
Division by zero at line 2
```

**Fix:** Don't divide by literal zero:
```c
int main() {
    int x = 10 / 2;
    return x;
}
```

**Note:** Division by zero with variables is only caught at runtime:
```c
int divide(int a, int b) {
    return a / b;  // OK at compile time
}

int main() {
    return divide(10, 0);  // Runtime error!
}
```

## Parse Errors

### Syntax Errors

#### Missing Semicolon

```c
int main() {
    int x = 5
    int y = 10;
    return x + y;
}
```

**Error:**
```
Parse error: unexpected token at line 3
```

**Fix:** Add semicolons (though GLaDOS is lenient):
```c
int x = 5;
int y = 10;
```

#### Mismatched Braces

```c
int main() {
    int x = 5;
    return x;
// Missing closing brace
```

**Error:**
```
Parse error: unexpected end of input
```

**Fix:** Close all braces:
```c
int main() {
    int x = 5;
    return x;
}
```

#### Invalid Token

```c
int main() {
    int x = 5 @ 3;  // @ is not a valid operator
    return x;
}
```

**Error:**
```
Parse error: unexpected character '@' at line 2
```

**Fix:** Use valid operators:
```c
int x = 5 + 3;
```

## Runtime Errors

These errors occur during program execution:

### Division by Zero (Runtime)

```c
int divide(int numerator, int denominator) {
    return numerator / denominator;
}

int main() {
    int result = divide(10, 0);
    return result;
}
```

**Error:**
```
Runtime error: Division by zero
```

**Fix:** Check for zero before dividing:
```c
int divide(int numerator, int denominator) {
    if (denominator == 0) {
        return 0;  // Or handle error appropriately
    }
    return numerator / denominator;
}
```

### Stack Overflow (Deep Recursion)

```c
int infinite(int x) {
    return infinite(x + 1);  // Never stops!
}
```

This may cause a stack overflow. Always ensure recursion has a base case.

## Error Message Format

GLaDOS error messages follow this format:

```
<Error Type>: <Description> at <Source Position>
```

Where:
- **Error Type**: Type error, Parse error, Runtime error, etc.
- **Description**: What went wrong
- **Source Position**: File location (line and column)

Example:
```
Type error in arithmetic at input:5:12: expected int but got bool
```

## Tips for Debugging

1. **Read the error message carefully** - it tells you exactly what's wrong
2. **Check the line number** - the error is usually on or near that line
3. **Look at variable types** - most errors are type mismatches
4. **Verify all code paths return** - use else branches
5. **Initialize variables** before using them
6. **Match function signatures** - check parameter counts and types
7. **Test incrementally** - compile often to catch errors early

## Exit Codes

GLaDOS uses standard Unix exit codes:
- **0**: Success, no errors
- **84**: Compilation or runtime error (EPITECH standard)
