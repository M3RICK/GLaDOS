# Formal Grammar (BNF)

This is the formal grammar specification for GLaDOS in Backus-Naur Form (BNF).

## Program Structure

```bnf
<program> ::= <top_level_list>

<top_level_list> ::= <top_level> | <top_level> <top_level_list>

<top_level> ::= <function> | <function_prototype>

<function> ::= <type> <identifier> "(" <parameters> ")" "{" <statement_list> "}"

<function_prototype> ::= <type> <identifier> "(" <parameters> ")" ";"
```

## Parameters

```bnf
<parameters> ::= ε | <parameter> | <parameter> "," <parameters>
<parameter> ::= <type> <identifier>
```

## Statements

```bnf
<statement_list> ::= ε | <statement> <statement_list>

<statement> ::= <declaration> ";"
              | <assignment> ";"
              | <if_statement>
              | <while_statement>
              | <for_statement>
              | <return_statement> ";"
```

### Statement Types

```bnf
<declaration> ::= <type> <identifier>
<assignment> ::= <identifier> "=" <expression>
<if_statement> ::= "if" "(" <expression> ")" "{" <statement_list> "}" <else_clause>
<else_clause> ::= ε | "else" "{" <statement_list> "}"
<while_statement> ::= "while" "(" <expression> ")" "{" <statement_list> "}"
<for_statement> ::= "for" "(" <for_init> ";" <for_condition> ";" <for_increment> ")" "{" <statement_list> "}"
<for_init> ::= ε | <declaration> | <assignment>
<for_condition> ::= ε | <expression>
<for_increment> ::= ε | <assignment>
<return_statement> ::= "return" <expression>
```

**Note on Semicolons:**

While the grammar shows semicolons after statements (declaration, assignment, return), they are **optional** in the actual implementation. The parser accepts statements with or without trailing semicolons.

However, semicolons are **required** in for loop syntax to separate the three clauses (`init`, `condition`, `increment`), as shown in the `<for_statement>` rule above.

## Expressions

```bnf
<expression> ::= <bool>
               | <number>
               | <float>
               | <identifier>
               | <unary_op> <expression>
               | <expression> <arith_op> <expression>
               | <expression> <bool_op> <expression>
               | "(" <expression> ")"
```

## Types and Operators

```bnf
<type> ::= "int" | "float" | "bool" | "void"
<unary_op> ::= "-" | "!"
<arith_op> ::= "+" | "-" | "*" | "/"
<bool_op> ::= "==" | "!=" | "<" | ">" | "<=" | ">=" | "&&" | "||"
```

## Literals and Identifiers

```bnf
<bool> ::= "true" | "false"
<number> ::= <digit>+
<float> ::= <digit>+ "." <digit>+
<identifier> ::= <letter> <identifier_rest>
<identifier_rest> ::= ε | <letter_or_digit> <identifier_rest>
<letter_or_digit> ::= <letter> | <digit>
<letter> ::= "a" | "b" | ... | "z" | "A" | "B" | ... | "Z" | "_"
<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
```

## Language Features

The grammar supports:
- **Functions** with parameters and return values
- **Function Prototypes** (forward declarations) for mutual recursion
- **Types**: `int`, `float`, `bool`, and `void`
- **Statements**: variable declaration, assignment, if/else, while, for, return
- **Expressions**: integers, floats, booleans, identifiers, unary operators, arithmetic operators, boolean operators
- **Comments**: C-style `//` line comments and `/* */` block comments

## Example Programs

### Simple Function
```c
int add(int a, int b) {
    return a + b;
}
```

### Control Flow
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
int factorial(int n) {
    int result = 1;
    while (n > 0) {
        result = result * n;
        n = n - 1;
    }
    return result;
}
```

### For Loops
```c
int sum(int n) {
    int total = 0;
    for (int i = 1; i <= n; i = i + 1) {
        total = total + i;
    }
    return total;
}
```

### Function Prototypes (Mutual Recursion)
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

### Float Arithmetic
```c
float circleArea(float radius) {
    float pi = 3.14159;
    return pi * radius * radius;
}
```

### Void Functions
```c
void printValue(int x) {
    if (x > 0) {
        return;
    }
}
```

### Unary Operators
```c
int negate(int x) {
    return -x;
}

bool invert(bool flag) {
    return !flag;
}
```

## Operator Precedence

From highest to lowest precedence:

1. **Unary**: `-`, `!` (right-to-left associativity)
2. **Multiplicative**: `*`, `/`
3. **Additive**: `+`, `-`
4. **Comparison**: `<=`, `>=`, `==`, `!=`, `<`, `>`
5. **Logical AND**: `&&`
6. **Logical OR**: `||`

Parentheses can be used to override precedence.
