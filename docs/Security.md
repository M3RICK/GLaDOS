# Security Review:

> A statically-typed language preventing common C bugs at compile time

---

## Quick Overview

| Safety Feature | Status |
|----------------|--------|
| Static type checking | ✓ Implemented |
| Initialization tracking | ✓ Implemented |
| Division by zero detection | ✓ Implemented |
| Return path verification | ✓ Implemented |

---

## The Problem: C's Unsafe Behaviors

### Type Confusion
```c
// C allows this - silent truncation
int x = 3.7;  // becomes 3, no warning

Uninitialized Variables
c

// C allows this - undefined behavior
int x;
return x;  // Could be anything

Division by Zero
c

// C allows this - runtime crash
int y = 10 / 0;

Missing Returns
c

// C allows this - returns garbage
int bad(int x) {
    if (x > 0) return 1;
    // Missing return here
}
```

## Our Solutions
 ## Type Safety

What we do: Enforce type correctness at compile time

Example error:
c

int x = true;

Error: Type mismatch at line 1
  expected: int
  got: bool

Initialization Tracking

What we do: Track which variables have been assigned

Example error:

```c

int x;
return x;

Error: Variable 'x' used before initialization at line 2

Division by Zero Detection
```

What we do: Catch literal zero denominators

Example error:

int y = 10 / 0;

Error: Division by zero at line 1

Limitation: Can't catch x / y where y is a variable that becomes zero
Return Path Verification

What we do: Ensure all code paths return a value

Example error:
```c
int bad(int x) {
    if (x > 0) {
        return 1;
    }
    // Missing else branch
}
```
Error: Function 'bad' must return a value on all code paths

## Design Rationale
Why Compile-Time Checks?

    Zero runtime overhead
    Errors caught before deployment
    Better developer experience

Why Not Catch Everything?

    Full constant propagation requires complex analysis
    False positives frustrate users
    Focus on high-impact, provable errors

## Limitations

We explicitly DO NOT catch:

**Integer Overflow**
- Would require runtime checks or different arithmetic semantics
- Impacts performance significantly
- Outside current scope

**Runtime Division by Zero**
- Requires tracking variable values through program execution
- Full constant propagation is complex and beyond scope
- We only catch literal zero denominators (e.g., `x / 0`)

**Complex Constant Folding**
- Would need sophisticated static analysis
- Example: `int a = 5 - 5; int b = 10 / a;` not caught
- Tradeoff between implementation complexity and value

**Memory Safety**
- No pointer types in current language version
- Would require borrow checker or garbage collection if added

---

## Comparison with C

| Feature | C Behavior | Our Language |
|---------|------------|--------------|
| Uninitialized variables | Undefined behavior, runtime bugs | Compile-time error with position |
| Type mismatches | Sometimes warns, often silent | Always compile-time error |
| Division by zero (literal) | Runtime crash | Compile-time error |
| Division by zero (variable) | Runtime crash | Runtime crash (same as C) |
| Missing return statements | Undefined behavior, returns garbage | Compile-time error |
| Integer overflow | Silent wraparound | Silent wraparound (same as C) |
| Runtime performance | Fast | Fast (zero overhead from checks) |

## Conclusion

We prevent entire classes of bugs that plague C programs, with zero runtime cost. While not catching everything, we focus on high-impact errors that are provably detectable at compile time.
