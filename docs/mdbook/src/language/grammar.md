# Formal Grammar (BNF)

This is the formal grammar specification for GLaDOS in Backus-Naur Form (BNF).

## Program Structure

```bnf
<program> ::= <function_list>

<function_list> ::= <function> | <function> <function_list>

<function> ::= <type> <identifier> "(" <parameters> ")" "{" <statement_list> "}"
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
              | <return_statement> ";"
```

### Statement Types

```bnf
<declaration> ::= <type> <identifier>
<assignment> ::= <identifier> "=" <expression>
<if_statement> ::= "if" "(" <expression> ")" "{" <statement_list> "}" <else_clause>
<else_clause> ::= ε | "else" "{" <statement_list> "}"
<while_statement> ::= "while" "(" <expression> ")" "{" <statement_list> "}"
<return_statement> ::= "return" <expression>
```

## Expressions

```bnf
<expression> ::= <bool>
               | <number>
               | <identifier>
               | <expression> <arith_op> <expression>
               | <expression> <bool_op> <expression>
```

## Types and Operators

```bnf
<type> ::= "int" | "bool"
<arith_op> ::= "+" | "-" | "*" | "/"
<bool_op> ::= "==" | "!=" | "<" | ">" | "<=" | ">=" | "&&" | "||"
```

## Literals and Identifiers

```bnf
<bool> ::= "true" | "false"
<identifier> ::= <letter> <identifier_rest>
<identifier_rest> ::= ε | <letter_or_digit> <identifier_rest>
<letter_or_digit> ::= <letter> | <digit>
<letter> ::= "a" | "b" | ... | "z" | "A" | "B" | ... | "Z" | "_"
<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
<number> ::= <digit> | <digit> <number>
```

## Language Features

The grammar supports:
- **Functions** with parameters and return values
- **Types**: `int` and `bool`
- **Statements**: variable declaration, assignment, if/else, while, return
- **Expressions**: numbers, booleans, identifiers, arithmetic operators, boolean operators
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

### Loops
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

## Operator Precedence

From highest to lowest precedence:

1. **Multiplicative**: `*`, `/`
2. **Additive**: `+`, `-`
3. **Comparison**: `<=`, `>=`, `==`, `!=`, `<`, `>`
4. **Logical AND**: `&&`
5. **Logical OR**: `||`

Parentheses can be used to override precedence.
