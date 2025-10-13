# Security Features

GLaDOS implements several compile-time and runtime safety features that prevent common programming errors.

## Overview

| Safety Feature | Detection Time | Status |
|----------------|---------------|--------|
| Static type checking | Compile-time | ✓ Implemented |
| Initialization tracking | Compile-time | ✓ Implemented |
| Division by zero (literal) | Compile-time | ✓ Implemented |
| Division by zero (variable) | Runtime | ✓ Implemented |
| Return path verification | Compile-time | ✓ Implemented |
| Function signature validation | Compile-time | ✓ Implemented |

## Compile-Time Safety

### 1. Static Type Checking

**What it does**: Ensures all operations use compatible types

**Prevents**: Type confusion, implicit conversions, type-based bugs

**Example error:**
```c
int x = true;
```

**Error message:**
```
Type error at line 1: expected int but got bool
```

**Implementation**: Type checker (`src/Compiler.hs`) infers expression types and verifies compatibility

**Benefits**:
- Catches errors before deployment
- No runtime type checking needed (zero overhead)
- Self-documenting code (types show intent)

### 2. Initialization Tracking

**What it does**: Tracks whether variables have been assigned before use

**Prevents**: Reading uninitialized memory, undefined behavior

**Example error:**
```c
int main() {
    int x;
    return x;  // ERROR: x not initialized
}
```

**Error message:**
```
Variable 'x' used before initialization at line 3
```

**Implementation**: Type checker maintains initialization status for each variable

**How it works:**
1. Declaration: Variable marked as `Uninitialized`
2. Assignment: Variable marked as `Initialized`
3. Usage: Check that variable is `Initialized`

**Example:**
```c
int x;              // x is Uninitialized
x = 42;             // x is now Initialized
return x;           // OK: x is Initialized

int y;              // y is Uninitialized
return y;           // ERROR: y is Uninitialized
```

### 3. Division by Zero Detection (Literal)

**What it does**: Detects division by literal zero at compile time

**Prevents**: Runtime crashes from obvious division errors

**Example error:**
```c
int main() {
    return 10 / 0;
}
```

**Error message:**
```
Division by zero at line 2
```

**Implementation**: Security analyzer (`src/Security.hs`) checks all division operations

**Limitations**: Only catches literal zeros, not variables

```c
// Caught at compile time
int x = 10 / 0;     // ERROR

// Not caught at compile time
int y = 0;
int z = 10 / y;     // Runtime error
```

### 4. Return Path Verification

**What it does**: Ensures all code paths return correct type

**Prevents**: Missing returns, returning uninitialized values, undefined behavior

**Example error:**
```c
int getValue(int x) {
    if (x > 0) {
        return 1;
    }
    // Missing return here!
}
```

**Error message:**
```
Function 'getValue' must return a value on all code paths
```

**Implementation**: Type checker analyzes all execution paths

**How it works:**
1. Check if function body contains explicit return
2. For if statements, verify both branches return
3. Verify returns have correct type

**Example:**
```c
// ✗ ERROR: Missing else branch
int bad1(int x) {
    if (x > 0) {
        return 1;
    }
}

// ✓ OK: Both branches return
int good1(int x) {
    if (x > 0) {
        return 1;
    } else {
        return 0;
    }
}

// ✓ OK: Return after if
int good2(int x) {
    if (x > 0) {
        return 1;
    }
    return 0;
}
```

### 5. Function Signature Validation

**What it does**: Verifies function calls match declarations

**Prevents**: Wrong argument count, wrong argument types, undefined functions

**Example errors:**
```c
int add(int a, int b) {
    return a + b;
}

int main() {
    return add(5);        // ERROR: Expected 2 args, got 1
    return add(true, 5);  // ERROR: Expected int, got bool
    return foo(5);        // ERROR: Undefined function 'foo'
}
```

**Error messages:**
```
Function 'add' expects 2 arguments but got 1 at line 6
Type mismatch in function argument at line 7: expected int but got bool
Undefined function 'foo' at line 8
```

**Implementation**: Type checker builds global environment of function signatures

**How it works:**
1. Pass 1: Collect all function signatures
2. Pass 2: Verify all function calls match signatures

## Runtime Safety

### 1. Division by Zero (Variable)

**What it does**: Checks for zero divisor at runtime

**Prevents**: Division by zero crashes

**Example:**
```c
int divide(int a, int b) {
    return a / b;  // Runtime check
}

int main() {
    return divide(10, 0);  // Runtime error
}
```

**Error message:**
```
Runtime error: Division by zero
```

**Implementation**: VM (`src/VM.hs`) checks divisor before division

**Performance**: Very low overhead (single integer comparison)

### 2. Stack Safety

**What it does**: Detects stack underflow and type errors

**Prevents**: Invalid stack operations, type confusion

**Example errors:**
```
Runtime error: Stack underflow
Runtime error: Type error in addition
```

**Implementation**: VM validates stack operations

### 3. Variable Safety

**What it does**: Checks variable exists before loading

**Prevents**: Undefined variable access

**Example error:**
```
Runtime error: Undefined variable 'x'
```

**Implementation**: VM checks environment before loading variable

## Memory Safety

**What it does**: Prevents memory errors by design

**How**: No pointers in the language

**Prevents**:
- Use-after-free
- Double free
- Buffer overflow
- Null pointer dereference
- Dangling pointers

**Trade-off**: Cannot do low-level systems programming

## Design Philosophy

### Zero Runtime Overhead (for compile-time checks)

All compile-time checks have **zero runtime cost**:

1. Type checking → No runtime type information
2. Initialization tracking → No runtime tracking
3. Return path verification → No runtime checks
4. Function signature validation → No runtime validation

Only **unavoidable** runtime checks:
- Division by zero (variable divisors)
- Stack operations (VM safety)

### Fail Fast

Errors are caught as early as possible:

1. **Parse time**: Syntax errors
2. **Compile time**: Type errors, initialization errors
3. **Runtime**: Division by zero (when unavoidable)

### Clear Error Messages

Every error message includes:
- Error type (Parse error, Type error, Runtime error)
- Line number
- Description of problem
- Expected vs actual (for type errors)

**Example:**
```
Type error at line 5: expected int but got bool
```

## Security Guarantees

GLaDOS provides the following guarantees:

### Type Soundness

**Guarantee**: Well-typed programs cannot have type errors at runtime

**How**: Complete type checking before IR generation

**Caveat**: VM still performs runtime type checks for safety

### Initialization Safety

**Guarantee**: Variables cannot be read before initialization

**How**: Compile-time tracking of initialization status

**Caveat**: Control flow is analyzed conservatively

### Return Safety

**Guarantee**: Functions always return correct type

**How**: All code paths verified to return

**Caveat**: Compiler may be conservative (require unnecessary returns)

### Operation Safety

**Guarantee**: Operators only applied to compatible types

**How**: Expression type inference and validation

**Caveat**: Integer overflow is not detected

## Limitations

### What GLaDOS Does NOT Prevent

1. **Integer Overflow**
   - Silent wraparound like C
   - Would require checked arithmetic or BigInt

2. **Runtime Division by Zero (Variable)**
   - Only literal zeros caught at compile time
   - Full constant propagation is complex

3. **Resource Leaks**
   - No file handles or resources yet
   - Would need RAII or garbage collection

4. **Logic Errors**
   - Type-safe but can still have bugs
   - Example: `return a + b` instead of `return a - b`

5. **Infinite Loops**
   - Loop termination is undecidable
   - Example: `while (true) {}`

6. **Stack Overflow**
   - Deep recursion can overflow call stack
   - No tail call optimization

## Future Enhancements

Potential future safety features:

1. **Checked Arithmetic**
   - Overflow detection for integer operations
   - Panic or wrap based on mode

2. **Constant Propagation**
   - Catch `int a = 0; int b = 10 / a;`
   - Requires more sophisticated analysis

3. **Bounds Checking**
   - If arrays are added
   - Prevent buffer overflow

4. **Resource Management**
   - RAII-style destructors
   - Automatic resource cleanup

5. **Null Safety**
   - Optional types with explicit handling
   - Prevent null dereference

## Testing Safety Features

### Test Coverage

GLaDOS has comprehensive tests for all safety features:

```bash
# Type safety tests
stack test --test-arguments="--match TypeChecker"

# Security tests
stack test --test-arguments="--match Security"

# Runtime safety tests
stack test --test-arguments="--match VM"
```

### Example Tests

```haskell
-- Test: Type mismatch
testTypeMismatch = "int main() { int x = true; return x; }"
expectError "Type error"

-- Test: Uninitialized variable
testUninitialized = "int main() { int x; return x; }"
expectError "used before initialization"

-- Test: Division by zero
testDivByZero = "int main() { return 10 / 0; }"
expectError "Division by zero"

-- Test: Missing return
testMissingReturn = "int getValue() { if (true) { return 1; } }"
expectError "must return a value"
```

## Comparison with Other Languages

See [Language Comparison](./language-comparison.md) for detailed comparison with C, C++, HolyC, Rust, and others.

## Conclusion

GLaDOS provides significant safety improvements over C with minimal complexity:

**✓ Compile-time safety:**
- Type safety
- Initialization tracking
- Return path verification
- Function signature validation

**✓ Runtime safety:**
- Division by zero checking
- Stack safety
- Variable safety

**✓ Design safety:**
- No pointers (memory safe)
- No null (no null dereference)
- Simple type system (easy to verify)

These features eliminate entire classes of bugs while maintaining:
- Zero runtime overhead (for compile-time checks)
- Fast compilation
- Simple language semantics
- Clear error messages

GLaDOS proves that significant safety can be achieved without complexity.
