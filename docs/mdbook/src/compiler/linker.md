# Linker System

The GLaDOS linker enables separate compilation and linking of multiple modules, allowing you to build large programs from independently compiled components.

**Added in:** Version 1.3

## Overview

The linker system provides:

- **Separate Compilation**: Compile modules independently
- **Symbol Resolution**: Resolve function calls across module boundaries
- **Import/Export Tracking**: Track dependencies between modules
- **Duplicate Detection**: Prevent duplicate function definitions
- **Missing Symbol Detection**: Catch undefined function references

## Architecture

### Object File Format

Object files (`.gbo` - GLaDOS Binary Object) contain:

```haskell
data ObjectFile = ObjectFile
  { objFunctions :: [RelocatableFunction]  -- Compiled functions
  , objExports :: [String]                 -- Functions this module provides
  , objImports :: [String]                 -- External functions this module needs
  }
```

**Key Components:**

- **Exports**: Functions defined in this module (available to other modules)
- **Imports**: External functions referenced by this module (must be provided by other modules)
- **Relocatable Functions**: Functions with symbolic (name-based) call references

### Relocatable Functions

Unlike fully compiled functions, relocatable functions use symbolic references:

```haskell
data RelocatableFunction = RelocatableFunction
  { rfName :: String
  , rfParams :: Int
  , rfLocals :: Int
  , rfCode :: [RelocatableInstr]           -- Instructions with symbolic calls
  }

data RelocatableInstr
  = Fixed Instruction                      -- Normal instruction
  | CallRef String                         -- Symbolic function call (name, not index)
```

**Why Relocatable?**

In a fully compiled program, function calls use integer indices (`Call 3`). In relocatable code, calls use symbolic names (`CallRef "helper"`). The linker resolves these symbolic references to actual indices during linking.

## Compilation Pipeline with Linker

### Traditional Single-Module Compilation

```
Source Code
    ↓
 [Parse]
    ↓
   AST
    ↓
[Type Check]
    ↓
[Compile to IR]
    ↓
IR Program
    ↓
  [VM]
    ↓
 Result
```

### Multi-Module Compilation with Linker

```
Module A Source          Module B Source
      ↓                        ↓
   [Parse]                  [Parse]
      ↓                        ↓
    AST                       AST
      ↓                        ↓
 [Type Check]             [Type Check]
      ↓                        ↓
[Compile to Object]    [Compile to Object]
      ↓                        ↓
  module_a.gbo             module_b.gbo
      ↓                        ↓
      └────────┬───────────────┘
               ↓
           [Linker]
               ↓
          IR Program
               ↓
             [VM]
               ↓
            Result
```

## Using the Linker

### Step 1: Write Modules with Function Prototypes

Function prototypes enable separate compilation by declaring functions before they're defined.

**module_a.c:**
```c
// Forward declaration for function in module_b
int helper(int x);

int process(int value) {
    return helper(value * 2);
}
```

**module_b.c:**
```c
// Forward declaration for function in module_a
int process(int value);

int helper(int x) {
    return x + 10;
}

int main() {
    return process(5);
}
```

### Step 2: Compile Modules to Object Files

Compile each module separately:

```bash
./glados --compile-object < module_a.c > module_a.gbo
./glados --compile-object < module_b.c > module_b.gbo
```

**What happens:**
1. Source code is parsed and type-checked
2. Compiled to IR with symbolic function calls
3. Exports and imports are tracked
4. Object file is serialized to disk

### Step 3: Link Object Files

Combine object files into executable program:

```bash
./glados --link module_a.gbo module_b.gbo -o program.gbc
```

**What the linker does:**
1. Load all object files
2. Build symbol table mapping function names to indices
3. Check for duplicate symbols
4. Check for undefined symbols
5. Resolve symbolic calls to actual indices
6. Find `main` function
7. Generate final IR program

### Step 4: Execute Linked Program

```bash
./glados --run program.gbc
```

Or link and execute in one step:

```bash
./glados --link-and-run module_a.gbo module_b.gbo
```

## Linking Process

### 1. Symbol Table Construction

The linker builds a global symbol table:

```haskell
buildSymbolTable :: [ObjectFile] -> Either String (Map String Int)
```

**Example:**

Given modules with functions:
- Module A: `["process", "utility"]`
- Module B: `["helper", "main"]`

Symbol table:
```haskell
{ "process" -> 0
, "utility" -> 1
, "helper"  -> 2
, "main"    -> 3
}
```

### 2. Duplicate Detection

Check for duplicate function definitions:

```haskell
checkDuplicates :: [String] -> Either String ()
```

**Error Example:**
```
Linker error: Duplicate symbols: ["process"]
```

This occurs when multiple modules define the same function.

### 3. Undefined Symbol Detection

Check that all imported symbols are defined:

```haskell
checkUndefinedSymbols :: [ObjectFile] -> Map String Int -> Either String ()
```

**Error Example:**
```
Linker error: Undefined symbols: ["helper"]
```

This occurs when a module calls a function that no module provides.

### 4. Symbol Resolution

Convert symbolic references to indices:

```haskell
resolveFunction :: Map String Int -> RelocatableFunction -> Either String CompiledFunction
```

**Before (Relocatable):**
```haskell
RelocatableFunction {
  rfName = "process",
  rfCode = [
    PushInt 5,
    CallRef "helper",  -- Symbolic reference
    Return
  ]
}
```

**After (Resolved):**
```haskell
CompiledFunction {
  funcName = "process",
  code = [
    PushInt 5,
    Call 2,            -- Resolved to index 2
    Return
  ]
}
```

### 5. Main Function Location

Find the entry point:

```haskell
findMainFunction :: Map String Int -> Either String Int
```

The linker ensures exactly one `main` function exists.

## Import/Export Tracking

### Automatic Import Detection

Imports are automatically detected from function calls:

**module_a.c:**
```c
int helper(int x);  // Prototype

int process(int value) {
    return helper(value);  // Calls helper
}
```

**Generated Object:**
```haskell
ObjectFile {
  objFunctions = [process],
  objExports = ["process"],
  objImports = ["helper"]  -- Automatically detected
}
```

### Export List

Exports include all top-level functions in the module:

**module_b.c:**
```c
int helper(int x) { return x + 10; }
int utility(int x) { return x * 2; }
int main() { return 0; }
```

**Generated Object:**
```haskell
ObjectFile {
  objFunctions = [helper, utility, main],
  objExports = ["helper", "utility", "main"],  -- All functions
  objImports = []  -- No external references
}
```

## Example: Multi-Module Program

### math.c - Math Utilities

```c
int square(int x) {
    return x * x;
}

int cube(int x) {
    return x * x * x;
}
```

### logic.c - Logic Utilities

```c
// Import from math module
int square(int x);

int isSquareLarge(int x) {
    int sq = square(x);
    return sq > 100;
}
```

### main.c - Main Program

```c
// Import from logic module
int isSquareLarge(int x);

// Import from math module
int cube(int x);

int main() {
    if (isSquareLarge(15)) {
        return cube(3);
    }
    return 0;
}
```

### Compilation and Linking

```bash
# Compile each module
./glados --compile-object < math.c > math.gbo
./glados --compile-object < logic.c > logic.gbo
./glados --compile-object < main.c > main.gbo

# Link all modules
./glados --link math.gbo logic.gbo main.gbo -o program.gbc

# Run
./glados --run program.gbc
```

**Dependency Graph:**
```
main.c → logic.c → math.c
main.c → math.c
```

**Symbol Resolution:**
```
math.gbo exports: ["square", "cube"]
logic.gbo exports: ["isSquareLarge"], imports: ["square"]
main.gbo exports: ["main"], imports: ["isSquareLarge", "cube"]

Linker resolves:
- logic's "square" → math's "square"
- main's "isSquareLarge" → logic's "isSquareLarge"
- main's "cube" → math's "cube"
```

## Linker Errors

### Duplicate Symbol Error

**Cause:** Multiple modules define the same function

**Example:**
```
Module A defines: int foo() { return 1; }
Module B defines: int foo() { return 2; }
```

**Error:**
```
Linker error: Duplicate symbols: ["foo"]
```

**Solution:** Rename one of the functions or remove the duplicate

### Undefined Symbol Error

**Cause:** Module calls function that no module provides

**Example:**
```c
// module_a.c
int helper(int x);  // Declared but never defined anywhere

int main() {
    return helper(5);
}
```

**Error:**
```
Linker error: Undefined symbols: ["helper"]
```

**Solution:** Provide the missing function in one of the modules

### No Main Function Error

**Cause:** None of the linked modules define `main`

**Error:**
```
Linker error: No main function found
```

**Solution:** Ensure one module defines `int main()`

## Object File Format

### Binary Serialization

Object files are serialized using Haskell's `Data.Binary`:

```haskell
saveObjectToFile :: FilePath -> ObjectFile -> IO (Either String ())
loadObjectFromFile :: FilePath -> IO (Either String ObjectFile)
```

**Format:** Binary encoded with:
- Function metadata (names, param counts, local counts)
- Relocatable instruction lists
- Export and import symbol lists

### File Extension

- **`.gbo`**: GLaDOS Binary Object (relocatable object file)
- **`.gbc`**: GLaDOS Bytecode (linked executable program)

## Implementation Details

### Module Structure

```
src/Linker/
├── Types.hs              # ObjectFile, RelocatableFunction types
├── ObjectCompiler.hs     # IR → Object conversion
├── Linker.hs            # Linking logic
└── BinaryInstances.hs   # Binary serialization instances
```

### Key Functions

**IR to Object:**
```haskell
irToObject :: IRProgram -> ObjectFile
```

Converts compiled IR to relocatable object file by:
1. Converting `Call <index>` to `CallRef <name>`
2. Extracting exports (all functions in module)
3. Detecting imports (external calls)

**Link Objects:**
```haskell
linkObjects :: [ObjectFile] -> Either String IRProgram
```

Links multiple objects into executable program by:
1. Building global symbol table
2. Checking for errors (duplicates, undefined symbols)
3. Resolving symbolic references
4. Finding main function
5. Generating final IR program

## Advantages of Separate Compilation

### 1. Faster Incremental Builds

Only recompile changed modules:
```bash
# Only module_a.c changed
./glados --compile-object < module_a.c > module_a.gbo
./glados --link math.gbo logic.gbo module_a.gbo -o program.gbc
```

### 2. Code Organization

Separate concerns into modules:
- `math.c`: Mathematical operations
- `io.c`: Input/output utilities
- `logic.c`: Business logic
- `main.c`: Program entry point

### 3. Team Collaboration

Different team members work on different modules without conflicts.

### 4. Reusable Libraries

Create reusable object files:
```bash
# Compile once
./glados --compile-object < stdlib.c > stdlib.gbo

# Link with multiple programs
./glados --link stdlib.gbo program1.gbo -o program1.gbc
./glados --link stdlib.gbo program2.gbo -o program2.gbc
```

## Limitations

### 1. No Circular Dependencies at Function Level

While modules can have circular dependencies (A imports from B, B imports from A), individual function calls cannot be circular without recursion.

### 2. Name-Based Symbol Resolution

The linker uses function names, so all exported functions must have unique names across all linked modules.

### 3. No Selective Exports

All top-level functions in a module are exported. There's no way to mark a function as "private" to the module.

## Best Practices

### 1. Use Function Prototypes

Always declare external functions with prototypes:
```c
int helper(int x);  // Good: Clear declaration

int process() {
    return helper(5);  // Linker will resolve this
}
```

### 2. Organize by Functionality

Group related functions into modules:
```
math.c      - Mathematical operations
string.c    - String utilities
io.c        - Input/output operations
main.c      - Program entry
```

### 3. Minimize Cross-Module Dependencies

Reduce coupling between modules to make linking simpler and builds faster.

### 4. One Main Function

Ensure only one module defines `main`:
```c
// main.c - Only this file has main
int main() {
    return 0;
}
```

## See Also

- [Compilation Overview](./overview.md): Full compilation pipeline
- [IR Generation](./ir-generation.md): How IR is generated
- [Function Prototypes](../language/syntax.md#function-prototypes-forward-declarations): Syntax for forward declarations
