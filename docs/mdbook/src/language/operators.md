# Operators

This page provides a complete reference for all operators in GLaDOS, including precedence, associativity, and type rules.

## Operator Categories

GLaDOS supports three categories of operators:

1. **Arithmetic Operators**: Mathematical operations on integers
2. **Comparison Operators**: Relational comparisons returning booleans
3. **Logical Operators**: Boolean logic operations

## Arithmetic Operators

All arithmetic operators require **both operands to be `int`** and produce an `int` result.

### Addition (`+`)

**Syntax**: `<expr> + <expr>`

**Type**: `int + int → int`

**Description**: Adds two integers

**Examples:**
```c
int a = 5 + 3;        // 8
int b = 10 + 20;      // 30
int c = -5 + 10;      // 5
int d = x + y;        // Sum of x and y
```

### Subtraction (`-`)

**Syntax**: `<expr> - <expr>`

**Type**: `int - int → int`

**Description**: Subtracts the second integer from the first

**Examples:**
```c
int a = 10 - 3;       // 7
int b = 5 - 10;       // -5
int c = x - 1;        // x decreased by 1
```

### Multiplication (`*`)

**Syntax**: `<expr> * <expr>`

**Type**: `int * int → int`

**Description**: Multiplies two integers

**Examples:**
```c
int a = 5 * 3;        // 15
int b = 10 * -2;      // -20
int c = n * n;        // n squared
```

### Division (`/`)

**Syntax**: `<expr> / <expr>`

**Type**: `int / int → int`

**Description**: Integer division (truncates toward zero)

**Examples:**
```c
int a = 10 / 2;       // 5
int b = 15 / 4;       // 3 (truncated)
int c = -10 / 3;      // -3 (truncated)
```

**Important Notes:**
- Division by zero with literal `0` is detected at **compile time**
- Division by zero with variables causes a **runtime error**

```c
int x = 10 / 0;       // ✗ COMPILE ERROR: Division by zero

int divide(int a, int b) {
    return a / b;     // ✓ Compiles, but...
}

int main() {
    return divide(10, 0);  // ✗ RUNTIME ERROR: Division by zero
}
```

**Workaround for safety:**
```c
int safeDivide(int a, int b) {
    if (b == 0) {
        return 0;  // Or handle error appropriately
    }
    return a / b;
}
```

## Comparison Operators

All comparison operators require **both operands to have the same type** and produce a `bool` result.

### Equal (`==`)

**Syntax**: `<expr> == <expr>`

**Type**: `T == T → bool` (where T is int or bool)

**Description**: Tests if two values are equal

**Examples:**
```c
bool a = 5 == 5;           // true
bool b = 10 == 3;          // false
bool c = true == false;    // false
bool d = x == y;           // true if x equals y
```

### Not Equal (`!=`)

**Syntax**: `<expr> != <expr>`

**Type**: `T != T → bool` (where T is int or bool)

**Description**: Tests if two values are not equal

**Examples:**
```c
bool a = 5 != 3;           // true
bool b = 10 != 10;         // false
bool c = true != false;    // true
```

### Less Than (`<`)

**Syntax**: `<expr> < <expr>`

**Type**: `int < int → bool`

**Description**: Tests if the first value is less than the second

**Examples:**
```c
bool a = 5 < 10;           // true
bool b = 10 < 5;           // false
bool c = 5 < 5;            // false
bool d = x < 100;          // true if x is less than 100
```

### Greater Than (`>`)

**Syntax**: `<expr> > <expr>`

**Type**: `int > int → bool`

**Description**: Tests if the first value is greater than the second

**Examples:**
```c
bool a = 10 > 5;           // true
bool b = 5 > 10;           // false
bool c = 5 > 5;            // false
```

### Less Than or Equal (`<=`)

**Syntax**: `<expr> <= <expr>`

**Type**: `int <= int → bool`

**Description**: Tests if the first value is less than or equal to the second

**Examples:**
```c
bool a = 5 <= 10;          // true
bool b = 10 <= 10;         // true
bool c = 15 <= 10;         // false
```

### Greater Than or Equal (`>=`)

**Syntax**: `<expr> >= <expr>`

**Type**: `int >= int → bool`

**Description**: Tests if the first value is greater than or equal to the second

**Examples:**
```c
bool a = 10 >= 5;          // true
bool b = 10 >= 10;         // true
bool c = 5 >= 10;          // false
```

## Logical Operators

All logical operators require **both operands to be `bool`** and produce a `bool` result.

### Logical AND (`&&`)

**Syntax**: `<expr> && <expr>`

**Type**: `bool && bool → bool`

**Description**: Returns true if **both** operands are true

**Truth Table:**
| Left  | Right | Result |
|-------|-------|--------|
| true  | true  | true   |
| true  | false | false  |
| false | true  | false  |
| false | false | false  |

**Examples:**
```c
bool a = true && true;         // true
bool b = true && false;        // false
bool c = false && false;       // false
bool d = (5 > 3) && (10 < 20); // true
bool e = (x > 0) && (x < 100); // true if x is between 1 and 99
```

**Short-Circuit Evaluation:**
GLaDOS uses short-circuit evaluation: if the left operand is `false`, the right operand is not evaluated.

```c
// If b is 0, the division is never performed
bool safe = (b != 0) && (a / b > 5);
```

### Logical OR (`||`)

**Syntax**: `<expr> || <expr>`

**Type**: `bool || bool → bool`

**Description**: Returns true if **either** operand is true

**Truth Table:**
| Left  | Right | Result |
|-------|-------|--------|
| true  | true  | true   |
| true  | false | true   |
| false | true  | true   |
| false | false | false  |

**Examples:**
```c
bool a = true || true;         // true
bool b = true || false;        // true
bool c = false || false;       // false
bool d = (x < 0) || (x > 100); // true if x is negative or greater than 100
```

**Short-Circuit Evaluation:**
If the left operand is `true`, the right operand is not evaluated.

## Operator Precedence

Operators are evaluated in the following order (highest to lowest precedence):

| Precedence | Operators | Description | Associativity |
|------------|-----------|-------------|---------------|
| 1 (highest) | `*`, `/` | Multiplicative | Left-to-right |
| 2 | `+`, `-` | Additive | Left-to-right |
| 3 | `<`, `>`, `<=`, `>=` | Relational | Left-to-right |
| 4 | `==`, `!=` | Equality | Left-to-right |
| 5 | `&&` | Logical AND | Left-to-right |
| 6 (lowest) | `||` | Logical OR | Left-to-right |

### Precedence Examples

```c
// Multiplication before addition
int a = 2 + 3 * 4;        // 2 + 12 = 14

// Comparison before logical AND
bool b = 5 > 3 && 10 < 20;  // true && true = true

// Logical AND before logical OR
bool c = true || false && false;  // true || false = true
```

### Using Parentheses

Parentheses `()` can override precedence:

```c
int a = (2 + 3) * 4;      // 5 * 4 = 20
bool b = 5 > (3 && 10);   // ERROR: Type mismatch
bool c = (true || false) && false;  // true && false = false
```

**Best Practice**: Use parentheses for clarity even when not strictly required:

```c
// Less clear
bool result = a > 0 && b < 10 || c == 5;

// More clear
bool result = ((a > 0) && (b < 10)) || (c == 5);
```

## Type Compatibility Rules

### Arithmetic Operators Type Rules

```c
// ✓ OK: Both operands are int
int x = 5 + 3;

// ✗ ERROR: Cannot mix int and bool
int y = 5 + true;
int z = false * 10;
```

### Comparison Operators Type Rules

```c
// ✓ OK: Both operands are int
bool a = 5 < 10;

// ✓ OK: Both operands are bool
bool b = true == false;

// ✗ ERROR: Cannot compare different types
bool c = 5 == true;
bool d = 10 < false;
```

### Logical Operators Type Rules

```c
// ✓ OK: Both operands are bool
bool a = true && false;
bool b = (5 > 3) || (10 == 10);

// ✗ ERROR: Operands must be bool
bool c = 5 && 10;
bool d = true && 42;
```

## Complete Examples

### Arithmetic Expression

```c
int calculate(int x, int y) {
    int sum = x + y;              // Addition
    int diff = x - y;             // Subtraction
    int product = x * y;          // Multiplication
    int quotient = x / y;         // Division
    int result = sum * 2 + diff;  // Mixed arithmetic
    return result;
}
```

### Comparison Chain

```c
bool isInRange(int x, int low, int high) {
    return (x >= low) && (x <= high);  // Comparison + Logical AND
}
```

### Complex Boolean Expression

```c
bool isValid(int age, bool hasLicense, bool isResident) {
    return (age >= 18) && hasLicense && isResident;
}
```

### Precedence in Action

```c
int main() {
    int x = 10;
    int y = 5;

    // Multiplication before addition
    int a = x + y * 2;            // 10 + 10 = 20

    // Comparison before logical AND
    bool b = x > 5 && y < 10;     // true && true = true

    // Override with parentheses
    int c = (x + y) * 2;          // 15 * 2 = 30

    return a + c;
}
```

## Common Mistakes

### Mixing Types

```c
// ✗ ERROR: Cannot use int in logical expression
if (x && y) {  // Wrong!
    return 1;
}

// ✓ OK: Use comparison
if ((x > 0) && (y > 0)) {  // Correct
    return 1;
}
```

### Division by Zero

```c
// ✗ COMPILE ERROR
int x = 100 / 0;

// ✗ RUNTIME ERROR
int divide(int a, int b) {
    return a / b;
}
divide(100, 0);

// ✓ OK: Check before dividing
int safeDivide(int a, int b) {
    if (b == 0) {
        return 0;
    }
    return a / b;
}
```

### Forgetting Parentheses

```c
// Unclear precedence
bool result = a > 0 && b < 10 || c == 5;

// Clear with parentheses
bool result = ((a > 0) && (b < 10)) || (c == 5);
```

## Operator Implementation

Operators are implemented in the VM using stack-based instructions:

- **Arithmetic**: `Add`, `Sub`, `Mul`, `Div`
- **Comparison**: `Eq`, `Neq`, `Lt`, `Gt`, `Lte`, `Gte`
- **Logical**: `And`, `Or`

All operators pop their operands from the stack and push the result back onto the stack.
