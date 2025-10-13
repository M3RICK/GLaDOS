# Compilation Overview

GLaDOS is a compiled language that transforms source code through multiple stages to produce executable bytecode. This page provides a high-level overview of the compilation pipeline.

## Compilation Pipeline

The GLaDOS compiler follows a multi-stage architecture:

```
Source Code (.c file)
       ↓
  [1. Lexing]
       ↓
    Tokens
       ↓
  [2. Parsing]
       ↓
  Abstract Syntax Tree (AST)
       ↓
  [3. Security Analysis]
       ↓
  Validated AST
       ↓
  [4. Type Checking]
       ↓
  Type-Safe AST
       ↓
  [5. IR Generation]
       ↓
  Intermediate Representation (IR)
       ↓
  [6. VM Execution]
       ↓
    Result (int)
```

## Stage 1: Lexical Analysis (Lexing)

**Input**: Source code (string)
**Output**: Token stream
**Module**: `Parser.hs` (using Megaparsec)

The lexer scans the source code and breaks it into **tokens** - the smallest meaningful units:

**Example:**
```c
int add(int a, int b) {
    return a + b;
}
```

**Tokens:**
```haskell
[ TInt, TIdentifier "add", TLParen
, TInt, TIdentifier "a", TComma
, TInt, TIdentifier "b", TRParen
, TLBrace, TReturn, TIdentifier "a"
, TPlus, TIdentifier "b", TSemicolon
, TRBrace
]
```

**Key Features:**
- Whitespace and comments are ignored
- Keywords are recognized (`int`, `bool`, `if`, `while`, `return`, etc.)
- Operators are tokenized (`+`, `-`, `*`, `/`, `==`, `&&`, etc.)
- Identifiers and literals are captured

## Stage 2: Syntactic Analysis (Parsing)

**Input**: Token stream
**Output**: Abstract Syntax Tree (AST)
**Module**: `Parser.hs` (Megaparsec parser combinators)

The parser consumes tokens and builds a tree structure representing the program's syntax:

**Example AST for `int add(int a, int b) { return a + b; }`:**

```haskell
Program
  [ Function
      (FuncDef
         { funcName = "add"
         , funcReturnType = TInt
         , funcParams = [("a", TInt), ("b", TInt)]
         , funcBody =
             [ Return
                 (BinOp Add
                   (Var "a")
                   (Var "b")
                 )
             ]
         }
      )
  ]
```

**Key Features:**
- Recursive descent parsing
- Operator precedence handling
- Error recovery and reporting
- Expression nesting support

## Stage 3: Security Analysis

**Input**: AST
**Output**: Validated AST
**Module**: `Security.hs`

The security analyzer performs static analysis to catch potential issues:

**Checks performed:**
1. **Division by zero detection** (for literal divisors)
2. **Function signature validation**
3. **Program structure validation** (requires `main` function)

**Example:**
```c
// ✗ SECURITY ERROR: Division by zero detected
int main() {
    return 10 / 0;  // Caught at compile time!
}
```

## Stage 4: Type Checking

**Input**: Validated AST
**Output**: Type-safe AST
**Module**: `Compiler.hs`

The type checker verifies type correctness throughout the program:

**Checks performed:**
1. **Type inference**: Determine expression types
2. **Type compatibility**: Verify operations use correct types
3. **Initialization tracking**: Ensure variables are initialized before use
4. **Return path analysis**: Verify all code paths return correct type
5. **Function call validation**: Check argument types and count

**Example:**
```c
// ✗ TYPE ERROR: expected int but got bool
int main() {
    int x = true;  // Type mismatch caught!
    return x;
}
```

**Type Environment:**
- **Global environment**: Function signatures
- **Local environment**: Variable types and initialization status
- **Return type tracking**: Expected return type for current function

## Stage 5: IR Generation

**Input**: Type-safe AST
**Output**: Intermediate Representation (IR)
**Module**: `Compiler.hs`

The compiler translates the AST into a lower-level IR suitable for execution:

**IR Instructions:**
- `Push <value>`: Push value onto stack
- `Pop`: Pop value from stack
- `Load <var>`: Load variable value
- `Store <var>`: Store value to variable
- `Add`, `Sub`, `Mul`, `Div`: Arithmetic operations
- `Eq`, `Neq`, `Lt`, `Gt`, `Lte`, `Gte`: Comparisons
- `And`, `Or`: Logical operations
- `Jump <label>`: Unconditional jump
- `JumpIfFalse <label>`: Conditional jump
- `Call <func> <arity>`: Function call
- `Return`: Return from function
- `Label <name>`: Jump target
- `PopN <count>`: Pop N values from stack

**Example:**
```c
int add(int a, int b) {
    return a + b;
}
```

**IR:**
```haskell
[ Label "add"
, Load "a"
, Load "b"
, Add
, Return
]
```

## Stage 6: VM Execution

**Input**: IR instructions
**Output**: Program result (int)
**Module**: `VM.hs`

The virtual machine executes IR instructions using a stack-based model:

**VM Components:**
- **Instruction Stack**: Current execution sequence
- **Value Stack**: Runtime values
- **Call Stack**: Function call frames
- **Environment**: Variable bindings
- **Program Counter**: Current instruction

**Execution Model:**
```
1. Fetch instruction
2. Decode operation
3. Execute operation (modify stack/environment)
4. Update program counter
5. Repeat until Return or end
```

**Example execution:**
```c
int main() {
    int x = 5;
    int y = 10;
    return x + y;
}
```

**VM trace:**
```
Push 5              Stack: [5]
Store "x"           Stack: [], Env: {x=5}
Push 10             Stack: [10]
Store "y"           Stack: [], Env: {x=5, y=10}
Load "x"            Stack: [5]
Load "y"            Stack: [5, 10]
Add                 Stack: [15]
Return              Result: 15
```

## Error Handling

Errors can occur at each stage:

### Lexing/Parsing Errors
```
Parse error: unexpected token at line 5
Parse error: unexpected end of input
```

### Security Errors
```
Division by zero at line 3
```

### Type Errors
```
Type error at line 5: expected int but got bool
Undefined variable 'x' at line 7
Function 'foo' expects 2 arguments but got 1 at line 10
```

### Runtime Errors
```
Runtime error: Division by zero
Runtime error: Stack underflow
```

## Compiler Architecture

### Module Structure

```
glados/
├── src/
│   ├── AST.hs              # AST data types
│   ├── Parser.hs           # Lexer and parser
│   ├── Security.hs         # Security analysis
│   ├── Compiler.hs         # Type checking + IR generation
│   ├── IR.hs               # IR data types
│   ├── VM.hs               # Virtual machine
│   └── Main.hs             # Entry point
```

### Data Flow

1. **Main.hs**: Reads source code from stdin
2. **Parser.hs**: Lexes and parses into AST
3. **Security.hs**: Validates AST for security issues
4. **Compiler.hs**: Type checks and generates IR
5. **VM.hs**: Executes IR and produces result
6. **Main.hs**: Prints result or error (exit code 0 or 84)

## Implementation Details

### Parser Combinators (Megaparsec)

GLaDOS uses Megaparsec for parsing:
- **Combinator-based**: Compose small parsers into larger ones
- **Error messages**: Detailed parse error reporting
- **Backtracking**: Try alternative parse paths
- **Lexer support**: Built-in token parsing utilities

Example parser combinator:
```haskell
functionDef :: Parser FuncDef
functionDef = do
  returnType <- typeParser
  name <- identifier
  params <- parens paramList
  body <- braces statementList
  return $ FuncDef name returnType params body
```

### Type Checking Algorithm

The type checker uses **environment-based inference**:

1. Build global environment with function signatures
2. For each function:
   - Initialize local environment
   - Track variable initialization status
   - Infer expression types bottom-up
   - Verify type constraints
   - Check all paths return

### IR Design

The IR is a **stack-based bytecode**:
- Simple to generate from AST
- Easy to execute in VM
- Compact representation
- No complex register allocation

### VM Design

The VM uses a **stack machine architecture**:
- All operations use the stack
- Function calls push new frames
- Return pops frames
- Simple and efficient

## Performance Characteristics

### Compilation Time

- **Lexing/Parsing**: O(n) where n is source length
- **Type Checking**: O(n) where n is AST size
- **IR Generation**: O(n) where n is AST size
- **Total**: Linear in source code size

### Runtime Performance

- **Stack operations**: O(1)
- **Variable access**: O(1) with environment map
- **Function calls**: O(1) for call overhead
- **Recursion**: Limited by stack size (Haskell stack)

## Exit Codes

GLaDOS follows standard EPITECH conventions:

- **0**: Success (program compiled and ran successfully)
- **84**: Error (parse error, type error, or runtime error)

## Next Steps

To learn more about each compilation stage:

- [Parsing](./parsing.md): Detailed parser implementation
- [Type Checking](./type-checking.md): Type system rules and algorithm
- [IR Generation](./ir-generation.md): IR instruction set and generation
- [VM Execution](./vm-execution.md): Virtual machine implementation
