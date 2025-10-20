# Syntax Guide

This guide provides a practical introduction to GLaDOS syntax with examples.

## Program Structure

Every GLaDOS program consists of one or more functions. The `main` function is the entry point.

```c
int main() {
    return 0;
}
```

## Functions

### Function Declaration

Functions are declared with a return type, name, parameters, and body:

```c
<return_type> <name>(<parameters>) {
    <statements>
}
```

**Example:**
```c
int add(int a, int b) {
    return a + b;
}
```

### Function Parameters

Functions can take zero or more parameters:

```c
// No parameters
int getFortyTwo() {
    return 42;
}

// Multiple parameters
int multiply(int x, int y) {
    return x * y;
}
```

### Return Statements

Every non-void function **must** return a value on all code paths:

```c
// ✓ GOOD - returns on all paths
int abs(int x) {
    if (x < 0) {
        return -x;
    } else {
        return x;
    }
}

// ✗ BAD - missing return in some paths
int bad(int x) {
    if (x > 0) {
        return 1;
    }
    // ERROR: no return here
}
```

## Variables

### Declaration

Variables must be declared with a type:

```c
int x;
bool flag;
```

### Initialization

Variables can be initialized when declared:

```c
int x = 10;
bool isReady = true;
```

### Assignment

Variables can be assigned after declaration:

```c
int x;
x = 5;
x = x + 1;  // x is now 6
```

## Types

GLaDOS supports two primitive types:

| Type | Description | Examples |
|------|-------------|----------|
| `int` | 64-bit signed integer | `0`, `42`, `-10`, `1000` |
| `bool` | Boolean value | `true`, `false` |

### Type Safety

GLaDOS is **strictly typed**. You cannot mix types:

```c
// ✗ ERROR: Type mismatch
int x = true;

// ✗ ERROR: Cannot add int and bool
int y = 5 + true;

// ✓ GOOD
int x = 42;
bool flag = false;
```

## Operators

### Arithmetic Operators

```c
int a = 10 + 5;   // Addition: 15
int b = 10 - 5;   // Subtraction: 5
int c = 10 * 5;   // Multiplication: 50
int d = 10 / 5;   // Division: 2
```

**Note:** Division by zero is detected at compile-time for literals:

```c
int x = 10 / 0;  // ✗ COMPILE ERROR: Division by zero
```

### Comparison Operators

All comparison operators return `bool`:

```c
bool a = 5 == 5;   // Equal: true
bool b = 5 != 3;   // Not equal: true
bool c = 5 < 10;   // Less than: true
bool d = 5 > 10;   // Greater than: false
bool e = 5 <= 5;   // Less or equal: true
bool f = 5 >= 10;  // Greater or equal: false
```

### Logical Operators

```c
bool a = true && false;   // AND: false
bool b = true || false;   // OR: true
```

### Operator Precedence

From highest to lowest:

1. `*`, `/` (Multiplicative)
2. `+`, `-` (Additive)
3. `<`, `>`, `<=`, `>=`, `==`, `!=` (Comparison)
4. `&&` (Logical AND)
5. `||` (Logical OR)

Use parentheses to override precedence:

```c
int x = 2 + 3 * 4;      // 14 (multiplication first)
int y = (2 + 3) * 4;    // 20 (parentheses first)
```

## Control Flow

### If Statements

```c
if (condition) {
    // executed if condition is true
}
```

### If-Else Statements

```c
if (condition) {
    // executed if true
} else {
    // executed if false
}
```

**Example:**
```c
int max(int a, int b) {
    if (a > b) {
        return a;
    } else {
        return b;
    }
}
```

### While Loops

```c
while (condition) {
    // repeated while condition is true
}
```

**Example:**
```c
int factorial(int n) {
    int result = 1;
    while (n > 0) {
        result = result * n;
        n = n - 1;
    }
    return result;
}
```

**Important:** The condition must be a `bool`:

```c
// ✗ ERROR: int where bool expected
while (x) {
    x = x - 1;
}

// ✓ GOOD
while (x > 0) {
    x = x - 1;
}
```

## Comments

GLaDOS supports C-style comments:

```c
// Single-line comment

/*
   Multi-line
   comment
*/

int x = 42;  // End-of-line comment
```

## Semicolons

Following in the footsteps of King Terry Davis and HolyC, **semicolons are completely optional** in GLaDOS. We know this might make some of you anxious, but the program will run either way - even if they're mixed up in the same function!

```c
// All of these are valid:

// With semicolons (if you like them)
int x = 5;
int y = 10;
return x + y;

// Without semicolons (HolyC style)
int x = 5
int y = 10
return x + y

// Mixed (because why not?)
int x = 5;
int y = 10
return x + y;

// Even in control structures
if (x > 0) {
    return x
}

// Or with semicolons if you prefer
if (x > 0) {
    return x;
}
```

Use them, don't use them, mix them - GLaDOS doesn't judge. Code the way Terry intended: free.

## Function Calls

Functions are called by name with arguments:

```c
int result = add(5, 3);
int answer = factorial(10);
```

### Recursion

Functions can call themselves:

```c
int fib(int n) {
    if (n <= 1) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}
```

## Expression Statements

Any expression can be used as a statement (useful for function calls with side effects):

```c
int print(int x) {
    return x;  // Pretend this prints
}

int main() {
    print(42);  // Expression statement
    return 0;
}
```

## Best Practices

1. **Always initialize variables** before use
2. **Use meaningful names** for functions and variables
3. **Keep functions small** and focused
4. **Add comments** to explain complex logic
5. **Handle all return paths** in non-void functions
6. **Use proper indentation** (4 spaces recommended)

## Common Mistakes

```c
// ✗ Using uninitialized variables
int x;
return x;  // ERROR: x not initialized

// ✗ Type mismatch
int getValue() {
    return true;  // ERROR: bool returned from int function
}

// ✗ Missing else branch return
int bad(int x) {
    if (x > 0) {
        return 1;
    }
    // ERROR: no return if x <= 0
}

// ✗ Wrong argument count
int add(int a, int b) { return a + b; }
int x = add(5);  // ERROR: expected 2 args, got 1
```
