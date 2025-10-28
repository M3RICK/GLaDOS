# GLaDOS Security Features

> A statically-typed, compiled language with compile-time safety guarantees and runtime protections

---

## Executive Summary

Our GLaDOS language implementation prioritizes **compile-time safety** to prevent entire classes of bugs that plague C programs. We enforce strict type checking, initialization tracking, and control flow verification before any code runs, resulting in zero runtime overhead for these safety features.

---

## Security Features Overview

| Safety Feature | Implementation | Detection Time | Status |
|----------------|----------------|----------------|--------|
| Static Type System | Full type checking with inference | Compile-time | ✓ Implemented |
| Initialization Tracking | Flow-sensitive analysis | Compile-time | ✓ Implemented |
| Division by Zero (literals) | AST analysis | Compile-time | ✓ Implemented |
| Return Path Verification | Control flow analysis | Compile-time | ✓ Implemented |
| Type Inference | `var` keyword with initialization | Compile-time | ✓ Implemented |
| Stack Safety | VM bounds checking | Runtime | ✓ Implemented |
| Function Call Safety | Argument count & type validation | Compile-time | ✓ Implemented |

---

## The Problem: Unsafe C Behaviors

### 1. Type Confusion
```c
// C allows this - silent truncation
int x = 3.7;  // becomes 3, no warning
char* p = (char*)123; // arbitrary casting
```

### 2. Uninitialized Variables
```c
// C allows this - undefined behavior
int x;
printf("%d", x);  // Could print anything
```

### 3. Division by Zero
```c
// C allows this - runtime crash
int result = 10 / 0;
```

### 4. Missing Return Statements
```c
// C allows this - returns garbage
int calculate(int x) {
    if (x > 0) return x * 2;
    // Oops, forgot the else case
}
```

---

## Our Solutions

### 1. Strong Static Type System

**What we do:** Enforce type correctness at compile time with no implicit conversions.

**Example - Type Mismatch Detection:**
```c
int x = true;  // Our language
```
**Error:**
```
Type error at line 1:5
  expected: int
  got: bool
```

**Example - Safe Type Inference:**
```c
var x = 42;        // Inferred as int
var y = 3.14;      // Inferred as float
var z = x + y;     // Error: cannot add int and float
```

### 2. Initialization Tracking

**What we do:** Track variable initialization through control flow paths using a flow-sensitive analysis.

**Example:**
```c
int x;
if (condition) {
    x = 10;
}
return x;  // Error: x may not be initialized
```
**Error:**
```
Variable 'x' used before initialization at line 4:8
```

**Safe Pattern:**
```c
int x = 0;  // Explicit initialization
if (condition) {
    x = 10;
}
return x;  // OK: x is definitely initialized
```

### 3. Division by Zero Detection

**What we do:** Catch literal zero denominators at compile time.

**Example:**
```c
int y = 10 / 0;
float z = 3.14 / 0.0;
```
**Error:**
```
Division by zero at line 1:9
```

**Runtime Protection:**
```c
int divisor = getUserInput();
int result = 10 / divisor;  // Runtime check in VM
```

### 4. Return Path Verification

**What we do:** Ensure all control paths in non-void functions return a value.

**Example:**
```c
int calculate(int x) {
    if (x > 0) {
        return x * 2;
    }
    // Missing else branch
}
```
**Error:**
```
Function 'calculate' must return a value on all code paths
```

**Correct Version:**
```c
int calculate(int x) {
    if (x > 0) {
        return x * 2;
    } else {
        return 0;
    }
}
```

### 5. Function Call Safety

**What we do:** Validate argument counts and types at compile time.

**Example:**
```c
int add(int a, int b) {
    return a + b;
}

int main() {
    int x = add(1);      // Error: wrong arg count
    int y = add(1, true); // Error: type mismatch
    return 0;
}
```

---

## Implementation Architecture

### Compile-Time Checks (Zero Runtime Cost)

1. **Parser** (`Parser/Core.hs`): Builds AST with source positions for error reporting
2. **Type Inference** (`Security/TypeInference.hs`): Resolves `var` declarations
3. **Type Checker** (`Security/TypeChecker.hs`): Validates types and initialization
4. **Compiler** (`Compiler/Core.hs`): Generates safe bytecode only for valid programs

### Runtime Safety (VM Layer)

1. **Stack Bounds Checking** (`VM/HelperFunc.hs`): Prevents stack overflow/underflow
2. **Local Variable Bounds** (`VM/HelperFunc.hs`): Array bounds checking for locals
3. **Division by Zero** (`VM/InstructionHandlers.hs`): Runtime check for dynamic values
4. **Type-Safe Operations**: VM maintains type tags (VInt, VBool, VFloat)

---

## Design Rationale

### Why Compile-Time Over Runtime?

1. **Zero Performance Overhead**: No runtime checks for proven-safe code
2. **Early Error Detection**: Bugs caught before deployment
3. **Better Developer Experience**: Immediate feedback during development
4. **Predictable Performance**: No hidden runtime costs

### Why Not Full Dependent Types?

We deliberately chose a practical middle ground:
- **What we have**: Strong static types with inference
- **What we don't**: Full dependent types or theorem proving
- **Rationale**: Balances safety with usability and compilation speed

---

## Limitations and Trade-offs

### What We DON'T Prevent:

1. **Integer Overflow**: Would require either runtime checks (performance cost) or different arithmetic semantics
2. **Runtime Division by Zero**: Only catch literal zeros; dynamic values checked at runtime
3. **Infinite Loops**: Halting problem - theoretically impossible to solve completely
4. **Stack Overflow from Deep Recursion**: Would need tail-call optimization (future work)

### Design Trade-offs:

- **No Implicit Conversions**: More verbose but prevents subtle bugs
- **Mandatory Initialization**: Requires explicit defaults but prevents undefined behavior
- **No Null/Pointers**: Limits expressiveness but eliminates entire bug classes

---

## Testing Our Security

### Type System Tests
```c
// All should fail compilation:
int x = 3.14;           // float to int
bool b = 42;            // int to bool
int y = true + false;   // bool arithmetic
```

### Initialization Tests
```c
// Should fail:
int x;
int y = x + 1;  // x not initialized

// Should pass:
int a = 0;
int b = a + 1;  // a is initialized
```

### Control Flow Tests
```c
// Should fail:
int risky(bool flag) {
    if (flag) return 42;
    // missing else
}

// Should pass:
int safe(bool flag) {
    if (flag) return 42;
    else return 0;
}
```

---

## Future Security Enhancements

### Planned Improvements:
1. **Tail Call Optimization**: Prevent stack overflow in recursive functions
2. **Bounds-Checked Arrays**: Safe indexing when arrays are added
3. **Pattern Matching**: Exhaustiveness checking for safer control flow
4. **Effect System**: Track side effects for pure functional programming

### Considered but Deferred:
1. **Gradual Typing**: Allow mixing static and dynamic typing
2. **Linear Types**: Resource management without GC
3. **Formal Verification**: Mathematical proofs of correctness

---

## Conclusion

Our language prevents entire classes of bugs that plague C programs through a carefully designed type system and compile-time analysis. By catching errors before runtime, we provide both safety and performance. While we don't catch everything (that would be impossible), we focus on high-impact, provably detectable errors that cause real problems in production systems.

The key insight: **Most bugs can be prevented by design, not just detected at runtime.**
