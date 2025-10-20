# Compilation Overview

> **WARNING**: This page is still being completed. Some of the information is false and filled with temporary dummy information.

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
  [3. IR Generation]
       ↓
  IR (Human-Readable Disassembly)
  (can be outputted with flag)
       ↓
  [4. Bytecode Generation]
       ↓
    Bytecode
  (can be outputted with flag)
       ↓
  [5. VM Execution]
       ↓
    Result
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

## Stage 3: IR Generation

**Input**: AST
**Output**: Human-Readable IR (Disassembly)
**Module**: `Compiler.hs`

The compiler translates the AST into an intermediate representation (IR) - a human-readable disassembly format that represents the program's instructions:

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

**IR (Human-Readable):**
```
Label "add"
Load "a"
Load "b"
Add
Return
```

This IR can be outputted using the appropriate compiler flag for debugging and inspection purposes.

## Stage 4: Bytecode Generation

**Input**: IR (Human-Readable)
**Output**: Bytecode
**Module**: `Compiler.hs`

The IR is then compiled into compact bytecode format suitable for efficient execution by the VM. This bytecode can also be outputted with a flag for inspection.

**Bytecode Format:**
- Binary representation of IR instructions
- Optimized for fast execution
- Can be saved to disk and loaded later

## Stage 5: VM Execution

**Input**: Bytecode
**Output**: Program result
**Module**: `VM.hs`

The virtual machine executes the bytecode using a stack-based model:

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

### Runtime Errors
```
Runtime error: Division by zero
Runtime error: Stack underflow
Runtime error: Undefined function
```

## Compiler Architecture

### Module Structure

```
glados/
├── src/
│   ├── AST.hs              # AST data types
│   ├── Parser.hs           # Lexer and parser
│   ├── Compiler.hs         # IR generation
│   ├── IR.hs               # IR data types
│   ├── Bytecode.hs         # Bytecode generation
│   ├── VM.hs               # Virtual machine
│   └── Main.hs             # Entry point
```

### Data Flow

1. **Main.hs**: Reads source code from stdin or file
2. **Parser.hs**: Lexes and parses into AST
3. **Compiler.hs**: Generates IR (human-readable disassembly)
4. **Bytecode.hs**: Compiles IR into bytecode
5. **VM.hs**: Executes bytecode and produces result
6. **Main.hs**: Prints result or error (exit code 0 or 84)

### Compiler Flags

The compiler supports flags to output intermediate stages:
- **IR flag**: Output human-readable IR disassembly
- **Bytecode flag**: Output compiled bytecode
- These are useful for debugging and understanding compilation

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

### IR Design

The IR is a **human-readable disassembly format**:
- Simple to generate from AST
- Easy to debug and inspect
- Clear representation of program flow
- Can be saved and examined independently

### Bytecode Format

Bytecode is the **binary compiled format**:
- Compact and efficient
- Fast to execute in VM
- Can be saved to disk
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
- **IR Generation**: O(n) where n is AST size
- **Bytecode Generation**: O(n) where n is IR size
- **Total**: Linear in source code size

### Runtime Performance

- **Stack operations**: O(1)
- **Variable access**: O(1) with environment map
- **Function calls**: O(1) for call overhead
- **Recursion**: Limited by stack size (Haskell stack)

## Exit Codes

GLaDOS follows standard EPITECH conventions:

- **0**: Success (program compiled and ran successfully)
- **84**: Error (parse error or runtime error)

## Next Steps

To learn more about each compilation stage:

- [Parsing](./parsing.md): Detailed parser implementation
- [IR Generation](./ir-generation.md): IR instruction set and generation
- [VM Execution](./vm-execution.md): Virtual machine implementation
