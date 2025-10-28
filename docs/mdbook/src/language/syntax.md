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
int getFortyTwo() {
    return 42;
}

int multiply(int x, int y) {
    return x * y;
}
```

### Return Statements

Every non-void function **must** return a value on all code paths:

```c
int abs(int x) {
    if (x < 0) {
        return -x;
    } else {
        return x;
    }
}

int bad(int x) {
    if (x > 0) {
        return 1;
    }
}
```

### Function Prototypes (Forward Declarations)

Functions can be declared before they are defined using prototypes. This enables mutual recursion and allows you to organize code with definitions after usage.

**Syntax:**
```c
<return_type> <function_name>(<parameters>);
```

**Examples:**
```c
int helper(int x);
bool isValid(int value);

int helper(int x) {
    return x * 2;
}

bool isValid(int value) {
    return helper(value) > 0;
}
```

**Use Cases:**
- Mutual recursion between functions
- Organize code with definitions after main
- Separate interface from implementation

**Mutual Recursion Example:**
```c
int isEven(int n);
int isOdd(int n);

int isEven(int n) {
    if (n == 0) {
        return 1;
    }
    return isOdd(n - 1);
}

int isOdd(int n) {
    if (n == 0) {
        return 0;
    }
    return isEven(n - 1);
}
```

**Important Notes:**
- Prototype signature must match the actual function definition
- Parameters can be named in prototypes, but names are ignored
- Prototypes are particularly useful for mutually recursive functions

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
x = x + 1;
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
int x = true;

int y = 5 + true;

int x = 42;
bool flag = false;
```

## Operators

### Arithmetic Operators

```c
int a = 10 + 5;
int b = 10 - 5;
int c = 10 * 5;
int d = 10 / 5;
```

**Note:** Division by zero is detected at compile-time for literals:

```c
int x = 10 / 0;
```

### Comparison Operators

All comparison operators return `bool`:

```c
bool a = 5 == 5;
bool b = 5 != 3;
bool c = 5 < 10;
bool d = 5 > 10;
bool e = 5 <= 5;
bool f = 5 >= 10;
```

### Logical Operators

```c
bool a = true && false;
bool b = true || false;
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
int x = 2 + 3 * 4;
int y = (2 + 3) * 4;
```

## Control Flow

### If Statements

```c
if (condition) {

}
```

### If-Else Statements

```c
if (condition) {

} else {

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
while (x) {
    x = x - 1;
}

while (x > 0) {
    x = x - 1;
}
```

### For Loops

```c
for (init; condition; increment) {

}
```

**Example:**
```c
int sum(int n) {
    int total = 0;
    for (int i = 1; i <= n; i = i + 1) {
        total = total + i;
    }
    return total;
}
```

**All clauses are optional:**
```c
for (;;) {
    if (done) {
        return;
    }
}

for (int i = 0; i < 10; i = i + 1) {

}

int i = 0;
for (; i < 10; i = i + 1) {

}

int i = 0;
for (; i < 10;) {
    i = i + 1;
}
```

**Init clause can be declaration or assignment:**
```c
for (int i = 0; i < 10; i = i + 1) {

}

int i;
for (i = 0; i < 10; i = i + 1) {

}
```

**Important:** The condition must be a `bool` (when provided):
```c
for (int i = 0; i < 10; i = i + 1) {

}

int i = 0;
for (; i;) {
    i = i - 1;
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

int x = 42;
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
    return x;
}

int main() {
    print(42);
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
int x;
return x;

int getValue() {
    return true;
}

int bad(int x) {
    if (x > 0) {
        return 1;
    }
}

int add(int a, int b) { return a + b; }
int x = add(5);
```
