# Operators

This page provides a complete reference for all operators in GLaDOS, including precedence, associativity, and type rules.

## Operator Categories

GLaDOS supports four categories of operators:

1. **Unary Operators**: Single-operand operations (negation, logical not)
2. **Arithmetic Operators**: Mathematical operations on integers and floats
3. **Comparison Operators**: Relational comparisons returning booleans
4. **Logical Operators**: Boolean logic operations

## Unary Operators

Unary operators take a single operand and produce a result.

### Negation (`-`)

**Syntax**: `-<expr>`

**Type**: `-int → int` or `-float → float`

**Description**: Negates a numeric value (changes sign)

**Examples:**
```c
int x = -5;
int y = -(10 + 3);
int z = -x;

float a = -3.14;
float b = -(2.5 * 2.0);
float c = -a;
```

**Type Rules:**
- Operand must be `int` or `float`
- Result type matches operand type
- Cannot negate boolean values

```c
int x = -5;
float y = -3.14;

bool z = -true;
```

### Logical NOT (`!`)

**Syntax**: `!<expr>`

**Type**: `!bool → bool`

**Description**: Inverts a boolean value

**Truth Table:**
| Input | Result |
|-------|--------|
| true  | false  |
| false | true   |

**Examples:**
```c
bool a = !true;
bool b = !false;
bool c = !(5 > 3);
bool d = !(x && y);
```

**Type Rules:**
- Operand must be `bool`
- Result is always `bool`
- Cannot negate numeric values

```c
bool x = !true;

int y = !5;
float z = !3.14;
```

## Arithmetic Operators

All arithmetic operators require **both operands to be `int`** and produce an `int` result.

### Addition (`+`)

**Syntax**: `<expr> + <expr>`

**Type**: `int + int → int`

**Description**: Adds two integers

**Examples:**
```c
int a = 5 + 3;
int b = 10 + 20;
int c = -5 + 10;
int d = x + y;
```

### Subtraction (`-`)

**Syntax**: `<expr> - <expr>`

**Type**: `int - int → int`

**Description**: Subtracts the second integer from the first

**Examples:**
```c
int a = 10 - 3;
int b = 5 - 10;
int c = x - 1;
```

### Multiplication (`*`)

**Syntax**: `<expr> * <expr>`

**Type**: `int * int → int`

**Description**: Multiplies two integers

**Examples:**
```c
int a = 5 * 3;
int b = 10 * -2;
int c = n * n;
```

### Division (`/`)

**Syntax**: `<expr> / <expr>`

**Type**: `int / int → int`

**Description**: Integer division (truncates toward zero)

**Examples:**
```c
int a = 10 / 2;
int b = 15 / 4;
int c = -10 / 3;
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
bool a = 5 == 5;
bool b = 10 == 3;
bool c = true == false;
bool d = x == y;
```

### Not Equal (`!=`)

**Syntax**: `<expr> != <expr>`

**Type**: `T != T → bool` (where T is int or bool)

**Description**: Tests if two values are not equal

**Examples:**
```c
bool a = 5 != 3;
bool b = 10 != 10;
bool c = true != false;
```

### Less Than (`<`)

**Syntax**: `<expr> < <expr>`

**Type**: `int < int → bool`

**Description**: Tests if the first value is less than the second

**Examples:**
```c
bool a = 5 < 10;
bool b = 10 < 5;
bool c = 5 < 5;
bool d = x < 100;
```

### Greater Than (`>`)

**Syntax**: `<expr> > <expr>`

**Type**: `int > int → bool`

**Description**: Tests if the first value is greater than the second

**Examples:**
```c
bool a = 10 > 5;
bool b = 5 > 10;
bool c = 5 > 5;
```

### Less Than or Equal (`<=`)

**Syntax**: `<expr> <= <expr>`

**Type**: `int <= int → bool`

**Description**: Tests if the first value is less than or equal to the second

**Examples:**
```c
bool a = 5 <= 10;
bool b = 10 <= 10;
bool c = 15 <= 10;
```

### Greater Than or Equal (`>=`)

**Syntax**: `<expr> >= <expr>`

**Type**: `int >= int → bool`

**Description**: Tests if the first value is greater than or equal to the second

**Examples:**
```c
bool a = 10 >= 5;
bool b = 10 >= 10;
bool c = 5 >= 10;
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
bool a = true && true;
bool b = true && false;
bool c = false && false;
bool d = (5 > 3) && (10 < 20);
bool e = (x > 0) && (x < 100);
```

**Short-Circuit Evaluation:**
GLaDOS uses short-circuit evaluation: if the left operand is `false`, the right operand is not evaluated.

```c
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
bool a = true || true;
bool b = true || false;
bool c = false || false;
bool d = (x < 0) || (x > 100);
```

**Short-Circuit Evaluation:**
If the left operand is `true`, the right operand is not evaluated.

## Operator Precedence

Operators are evaluated in the following order (highest to lowest precedence):

| Precedence | Operators | Description | Associativity |
|------------|-----------|-------------|---------------|
| 1 (highest) | `-`, `!` | Unary (negation, not) | Right-to-left |
| 2 | `*`, `/` | Multiplicative | Left-to-right |
| 3 | `+`, `-` | Additive | Left-to-right |
| 4 | `<`, `>`, `<=`, `>=` | Relational | Left-to-right |
| 5 | `==`, `!=` | Equality | Left-to-right |
| 6 | `&&` | Logical AND | Left-to-right |
| 7 (lowest) | `||` | Logical OR | Left-to-right |

### Precedence Examples

```c
int a = 2 + 3 * 4;

bool b = 5 > 3 && 10 < 20;

bool c = true || false && false;

int d = -5 + 3;

bool e = !true && false;

int f = -2 * 3;
```

### Using Parentheses

Parentheses `()` can override precedence:

```c
int a = (2 + 3) * 4;
bool b = 5 > (3 && 10);
bool c = (true || false) && false;
```

**Best Practice**: Use parentheses for clarity even when not strictly required:

```c
bool result = a > 0 && b < 10 || c == 5;

bool result = ((a > 0) && (b < 10)) || (c == 5);
```

## Type Compatibility Rules

### Arithmetic Operators Type Rules

```c
int x = 5 + 3;

int y = 5 + true;
int z = false * 10;
```

### Comparison Operators Type Rules

```c
bool a = 5 < 10;

bool b = true == false;

bool c = 5 == true;
bool d = 10 < false;
```

### Logical Operators Type Rules

```c
bool a = true && false;
bool b = (5 > 3) || (10 == 10);

bool c = 5 && 10;
bool d = true && 42;
```

## Complete Examples

### Arithmetic Expression

```c
int calculate(int x, int y) {
    int sum = x + y;
    int diff = x - y;
    int product = x * y;
    int quotient = x / y;
    int result = sum * 2 + diff;
    return result;
}
```

### Comparison Chain

```c
bool isInRange(int x, int low, int high) {
    return (x >= low) && (x <= high);
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

    int a = x + y * 2;

    bool b = x > 5 && y < 10;

    int c = (x + y) * 2;

    return a + c;
}
```

## Common Mistakes

### Mixing Types

```c
if (x && y) {
    return 1;
}

if ((x > 0) && (y > 0)) {
    return 1;
}
```

### Division by Zero

```c
int x = 100 / 0;

int divide(int a, int b) {
    return a / b;
}
divide(100, 0);

int safeDivide(int a, int b) {
    if (b == 0) {
        return 0;
    }
    return a / b;
}
```

### Forgetting Parentheses

```c
bool result = a > 0 && b < 10 || c == 5;

bool result = ((a > 0) && (b < 10)) || (c == 5);
```

## Operator Implementation

Operators are implemented in the VM using stack-based instructions:

- **Arithmetic**: `Add`, `Sub`, `Mul`, `Div`
- **Comparison**: `Eq`, `Neq`, `Lt`, `Gt`, `Lte`, `Gte`
- **Logical**: `And`, `Or`

All operators pop their operands from the stack and push the result back onto the stack.
