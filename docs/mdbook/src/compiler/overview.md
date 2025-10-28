# Compilation Overview

GLaDOS is a compiled language that transforms source code through multiple stages to produce executable bytecode. This page provides a high-level overview of the compilation pipeline.

## Compilation Pipeline

The GLaDOS compiler follows a multi-stage architecture:

```
Source Code (.c file)
       ↓
  [1. Parsing]
  (Lexing + Parsing combined via Megaparsec)
       ↓
  Abstract Syntax Tree (AST)
       ↓
  [2. Type Checking]
  (Security/TypeChecker.hs)
       ↓
  Type-Checked AST
       ↓
  [3. IR Compilation]
  (Compiler/Core.hs)
       ↓
  IR Program (CompiledFunction list)
  (can be outputted with --ir flag)
       ↓
  [4. Bytecode Serialization] (Optional)
  (For saving to .gbc files)
       ↓
    Bytecode (.gbc)
       ↓
  [5. Linking] (Optional)
  (For multi-module programs)
       ↓
  Linked Program
       ↓
  [6. VM Execution]
  (VM/Interpreter.hs)
       ↓
    Result
```

**Key Points:**
- **Lexing and parsing** are combined in a single stage using Megaparsec
- **Type checking** is a critical stage that validates type safety before compilation
- **IR compilation** generates WebAssembly-like indexed bytecode
- **Bytecode serialization** is optional, only needed for saving to disk
- **Linking** is optional, only needed for multi-module programs

## Stage 1: Parsing (Lexing + Parsing)

**Input**: Source code (string)
**Output**: Abstract Syntax Tree (AST)
**Module**: `Parser/Core.hs` (using Megaparsec)

Megaparsec combines lexical analysis and parsing into a single stage. The parser directly consumes source code and produces an AST, handling tokenization internally.

**What happens internally:**

1. **Lexical Analysis**: Source code is broken into tokens (keywords, operators, identifiers, literals)
2. **Syntactic Analysis**: Tokens are structured into an AST tree

**Lexer Features:**
- Whitespace and comments are ignored
- Keywords are recognized (`int`, `float`, `bool`, `void`, `if`, `while`, `for`, `return`, etc.)
- Operators are tokenized (`+`, `-`, `*`, `/`, `==`, `&&`, `!`, etc.)
- Identifiers and literals are captured with source positions

**Parser Features:**
- Recursive descent parsing with parser combinators
- Operator precedence handling
- Error recovery and detailed error reporting
- Expression nesting support

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

## Stage 2: Type Checking

**Input**: Abstract Syntax Tree (AST)
**Output**: Type-Checked AST (or type error)
**Module**: `Security/TypeChecker.hs`

**Critical Stage:** The type checker validates the program's type safety before compilation. This prevents invalid programs from being compiled and executed.

**Type Checking Operations:**

1. **Type Inference**: Determine the type of every expression
2. **Type Compatibility**: Verify operations use correct types
3. **Initialization Tracking**: Ensure variables are initialized before use
4. **Return Path Analysis**: Verify all code paths return correct type
5. **Function Validation**: Check function calls and signatures

**Example Type Errors Caught:**
```c
int x = true;

int y;
return y;

if (x > 0) {
    return 1;
}

int result = add(5);
```

**Why "Security" Module?**
Type checking is a security feature that prevents:
- Type confusion vulnerabilities
- Use of uninitialized memory
- Missing return values causing undefined behavior
- Function signature mismatches

See [Type Checking](./type-checking.md) for detailed documentation.

## Stage 3: IR Compilation

**Input**: Type-Checked AST
**Output**: IR Program (list of CompiledFunction)
**Module**: `Compiler/Core.hs`

The compiler translates the validated AST into an intermediate representation (IR) - a WebAssembly-like indexed bytecode:

**Key Architectural Features:**
- **Indexed Locals**: Variables accessed by integer index, not name
- **Type-Specific Instructions**: Separate instructions for int, float, bool operations
- **Offset-Based Jumps**: Relative jumps by instruction offset, not labels
- **Function Indexing**: Functions called by index, not name

**IR Instruction Categories:**

1. **Stack Operations**: `PushInt`, `PushFloat`, `PushBool`, `Pop`
2. **Local Variables**: `GetLocal <index>`, `SetLocal <index>`
3. **Arithmetic**: `AddInt`, `AddFloat`, `SubInt`, `SubFloat`, `MulInt`, `MulFloat`, `DivInt`, `DivFloat`
4. **Unary**: `NegInt`, `NegFloat`, `NotBool`
5. **Comparison**: `EqInt`, `EqFloat`, `LtInt`, `LtFloat`, etc.
6. **Logical**: `AndBool`, `OrBool`
7. **Control Flow**: `Jump <offset>`, `JumpIfFalse <offset>`, `Call <funcIndex>`
8. **Utility**: `Return`, `Halt`

**Example:**
```c
int add(int a, int b) {
    return a + b;
}
```

**Compiled IR:**
```haskell
CompiledFunction {
  funcName = "add",
  paramCount = 2,        -- a and b are params
  localVarCount = 0,     -- no local variables
  code = [
    GetLocal 0,          -- Get parameter 'a' (index 0)
    GetLocal 1,          -- Get parameter 'b' (index 1)
    AddInt,              -- Type-specific int addition
    Return
  ]
}
```

**Local Variable Indexing:**
- Parameters are indexed first: `0, 1, 2, ...`
- Local variables follow: `paramCount, paramCount+1, ...`

See [IR Generation](./ir-generation.md) for complete instruction set documentation.

## Stage 4: Bytecode Serialization (Optional)

**Input**: IR Program (CompiledFunction list)
**Output**: Bytecode file (.gbc)
**Module**: `Bytecode/Serialize.hs`

This stage is **optional** and only used when saving compiled programs to disk.

**Purpose:**
- Save compiled IR to `.gbc` (GLaDOS Bytecode) files
- Enable separate compilation and distribution
- Load pre-compiled programs for faster execution

**Bytecode Format:**
- Binary serialization of IR instructions
- Compact representation for storage
- Can be loaded and executed without recompilation

**Usage:**
```bash
./glados --compile < program.c > program.gbc
./glados --run program.gbc
```

## Stage 5: Linking (Optional)

**Input**: Multiple compiled modules (.gbc files)
**Output**: Linked IR Program
**Module**: Linker system (v1.3+)

This stage is **optional** and only used for multi-module programs.

**Purpose:**
- Combine separately compiled modules
- Resolve external function references
- Create single executable program

**How it works:**
1. Each module compiled with function prototypes for external references
2. Linker resolves function calls across module boundaries
3. Produces single unified IR program

**Usage:**
```bash
./glados --link module1.gbc module2.gbc -o program.gbc
```

See [Linker Documentation](./linker.md) for details (when available).

## Stage 6: VM Execution

**Input**: IR Program (CompiledFunction list)
**Output**: Program result (exit code)
**Module**: `VM/Interpreter.hs`

The virtual machine executes the IR program using a stack-based model:

**VM State:**
```haskell
data VMState = VMState
  { stack :: [Value]           -- Operand stack
  , locals :: [Value]          -- Indexed local variables
  , pc :: Int                  -- Program counter
  , callStack :: [CallFrame]   -- Function call frames
  }
```

**Key Features:**
- **Indexed Locals**: Variables stored in array by index, not Map by name
- **Type Checking**: Stack operations validate types (popInt, popFloat, popBool)
- **Stack-Based**: All operations use operand stack
- **Call Frames**: Each function call creates isolated frame

**Execution Model:**
```
1. Fetch instruction at PC
2. Execute instruction (modify stack/locals)
3. Increment PC
4. Repeat until Return or Halt
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
PushInt 5           Stack: [5]          Locals: []
SetLocal 0          Stack: []           Locals: [5]        (x = 5)
PushInt 10          Stack: [10]         Locals: [5]
SetLocal 1          Stack: []           Locals: [5, 10]    (y = 10)
GetLocal 0          Stack: [5]          Locals: [5, 10]    (load x)
GetLocal 1          Stack: [5, 10]      Locals: [5, 10]    (load y)
AddInt              Stack: [15]         Locals: [5, 10]    (5 + 10)
Return              Result: 15
```

See [VM Execution](./vm-execution.md) for complete execution details.

## Error Handling

Errors can occur at each stage:

### Parsing Errors
```
Parse error at line 5, column 10: unexpected token '{'
Parse error at line 10: unexpected end of input, expecting statement
```

### Type Checking Errors
```
Type error at line 8: expected int but got bool
Type error at line 12: variable 'x' used before initialization
Type error at line 15: function 'add' expects 2 arguments but got 1
Type error: function must return a value on all code paths
```

### Compilation Errors
```
Compile error: division by zero at line 20
```

### Runtime Errors
```
Runtime error: Division by zero
Runtime error: Stack underflow
Runtime error: Type mismatch in operation
```

## Compiler Architecture

### Module Structure

GLaDOS uses a modularized architecture with separate directories for each major component:

```
glados/
├── src/
│   ├── AST/
│   │   └── AST.hs                # AST data types
│   ├── Parser/
│   │   ├── Core.hs               # Main parser export
│   │   ├── Lexer.hs              # Lexer utilities
│   │   ├── Program.hs            # Program parser
│   │   ├── Statement.hs          # Statement parsers
│   │   └── Expression.hs         # Expression parsers
│   ├── Security/
│   │   ├── Environment.hs        # Type checking environment
│   │   ├── TypeChecker.hs        # Type checking implementation
│   │   └── Types.hs              # Type checking data types
│   ├── Compiler/
│   │   ├── Core.hs               # Main compiler
│   │   ├── Environment.hs        # Variable/function tables
│   │   ├── Expr.hs               # Expression compilation
│   │   ├── Statement.hs          # Statement compilation
│   │   └── Function.hs           # Function compilation
│   ├── IR/
│   │   └── Types.hs              # IR instruction types
│   ├── Bytecode/
│   │   └── Serialize.hs          # Bytecode serialization
│   ├── VM/
│   │   ├── Interpreter.hs        # VM execution
│   │   ├── HelperFunc.hs         # VM helper functions
│   │   └── InstructionHandlers.hs # Instruction execution handlers
│   ├── Error/
│   │   └── Types.hs              # Error types
│   └── Main.hs                   # Entry point
```

### Data Flow

1. **Main.hs**: Reads source code from stdin or file
2. **Parser/Core.hs**: Parses source code into AST
3. **Security/TypeChecker.hs**: Validates type safety
4. **Compiler/Core.hs**: Compiles AST to IR (indexed bytecode)
5. **Bytecode/Serialize.hs** (optional): Serializes IR to .gbc file
6. **Linker** (optional): Links multiple modules together
7. **VM/Interpreter.hs**: Executes IR program
8. **Main.hs**: Returns result (exit code 0 for success, 84 for error)

### Compiler Modes

The GLaDOS compiler supports multiple modes of operation:

| Mode | Usage | Description |
|------|-------|-------------|
| **Default** | `./glados < program.c` | Read from stdin, compile and execute |
| **AST Mode** | `./glados --ast < program.c` | Parse and display the Abstract Syntax Tree |
| **IR Mode** | `./glados --ir < program.c` | Parse, compile and display human-readable IR |
| **Compile Mode** | `./glados --compile < program.c > out.gbc` | Compile to bytecode and write to stdout |
| **Run Mode** | `./glados --run program.gbc` | Execute bytecode from FILE |
| **Help** | `./glados --help` | Display help message |

**Examples:**
```bash
# Compile and execute
./glados < program.c

# Show the AST for debugging
./glados --ast < program.c

# Show human-readable IR
./glados --ir < program.c

# Compile to bytecode file
./glados --compile < program.c > program.gbc

# Execute bytecode
./glados --run program.gbc
```

These modes are useful for debugging, understanding the compilation process, and creating distributable bytecode

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
