# Design Decisions

> Why we chose specific technologies and design approaches for GLaDOS

---

## Overview

This document explains the rationale behind our technology and design choices. Each decision was made to balance learning goals, development speed, safety, and maintainability.

---

## Build System: Stack

### What is Stack?

Stack is a cross-platform build tool for Haskell projects that provides reproducible builds and dependency management.

### Why Stack?

| Criterion | Stack | Cabal | Raw GHC | Our Choice |
|-----------|-------|-------|---------|------------|
| **Reproducible Builds** | ✓ LTS snapshots | Partial (with freeze files) | ✗ Manual | **Stack** |
| **Dependency Management** | ✓ Automatic resolution | ✓ Manual constraints | ✗ None | **Stack** |
| **Ease of Setup** | ✓ One command | Moderate | Complex | **Stack** |
| **Project Isolation** | ✓ Separate environments | ✓ With sandboxes | ✗ Global | **Stack** |
| **Learning Curve** | Low | Moderate | High | **Stack** |

**Key Benefits:**
1. **LTS (Long Term Support) Snapshots**: We use LTS 22.6, which ensures all dependencies work together
2. **Predictable Builds**: Same LTS = same behavior across team members' machines
3. **Simple Commands**: `stack build`, `stack test`, `stack run` - intuitive workflow
4. **Integrated Tools**: Built-in GHC management, testing, benchmarking

**Why Not Cabal?**
- Cabal is more flexible but requires more configuration
- Stack's opinionated approach reduces decision fatigue
- LTS snapshots eliminate "dependency hell"

**Why Not Raw GHC?**
- No dependency management
- Manual package installation is error-prone
- No reproducible builds across machines

**Decision:** Stack provides the best developer experience for a team project with educational goals.

---

## Parser Library: Megaparsec

### What is Megaparsec?

Megaparsec is a modern parser combinator library for Haskell, successor to Parsec.

### Why Megaparsec?

| Criterion | Megaparsec | Parsec | Alex/Happy | Parser Generators | Our Choice |
|-----------|------------|--------|------------|-------------------|------------|
| **Error Messages** | Excellent | Good | Poor | Poor | **Megaparsec** |
| **Composability** | ✓ Combinator-based | ✓ Combinator-based | ✗ Separate tools | ✗ Generated code | **Megaparsec** |
| **Learning Curve** | Low | Low | Moderate | High | **Megaparsec** |
| **Flexibility** | High | High | Low | Low | **Megaparsec** |
| **Type Safety** | ✓ Strong | ✓ Strong | Moderate | Weak | **Megaparsec** |
| **Maintenance** | Active | Legacy | Active | Varies | **Megaparsec** |

**Key Benefits:**
1. **Parser Combinators**: Build complex parsers from simple ones
   ```haskell
   functionDef = do
     returnType <- typeParser
     name <- identifier
     params <- parens paramList
     body <- braces statementList
     return $ FuncDef name returnType params body
   ```

2. **Excellent Error Messages**: Detailed parse errors with source positions
   ```
   Parse error at line 5, column 10:
     unexpected '{'
     expecting ';', ')', or identifier
   ```

3. **Built-in Lexer Support**: `Text.Megaparsec.Char.Lexer` handles whitespace and comments automatically

4. **Type-Safe**: Haskell's type system catches parser bugs at compile time

**Why Not Parsec?**
- Megaparsec is the modern successor with better error messages
- More active development and community support
- Better performance on large inputs

**Why Not Alex/Happy?**
- Separate lexer and parser generators add complexity
- Less flexible for language experimentation
- Generated code is harder to understand and debug
- No composability - can't easily reuse parser components

**Why Not Parser Generators (Yacc, Bison)?**
- Require learning separate DSL syntax
- Generated code is opaque
- Poor error messages by default
- Doesn't leverage Haskell's type system

**Decision:** Megaparsec's composability and error messages make it ideal for educational purposes and rapid language iteration.

---

## Testing Framework: Hspec

### What is Hspec?

Hspec is a BDD (Behavior-Driven Development) testing framework for Haskell, inspired by Ruby's RSpec.

### Why Hspec?

| Criterion | Hspec | HUnit | QuickCheck | Tasty | Our Choice |
|-----------|-------|-------|------------|-------|------------|
| **Readability** | ✓ Descriptive | Moderate | Low | Moderate | **Hspec** |
| **BDD Style** | ✓ `describe`/`it` | ✗ Assertions | ✗ Properties | Mixed | **Hspec** |
| **Output Formatting** | Excellent | Basic | Basic | Good | **Hspec** |
| **Learning Curve** | Low | Low | Moderate | Moderate | **Hspec** |
| **Property Testing** | ✓ Integrates QuickCheck | ✗ | ✓ Native | ✓ | **Hspec** |
| **Parallel Execution** | ✓ | ✗ | ✗ | ✓ | **Hspec** |

**Key Benefits:**
1. **Readable Test Structure**:
   ```haskell
   describe "Type Checker" $ do
     it "detects type mismatches" $ do
       typeCheck "int x = true;" `shouldSatisfy` isLeft

     it "allows correct assignments" $ do
       typeCheck "int x = 42;" `shouldSatisfy` isRight
   ```

2. **Descriptive Output**:
   ```
   Type Checker
     ✓ detects type mismatches
     ✓ allows correct assignments
     ✓ tracks variable initialization
     ✗ FAILED: should catch division by zero
   ```

3. **QuickCheck Integration**: Can combine example-based and property-based testing
   ```haskell
   it "type checks are commutative" $ property $
     \expr1 expr2 -> typeCheck (expr1 + expr2) == typeCheck (expr2 + expr1)
   ```

**Why Not HUnit?**
- Less descriptive test output
- No BDD-style organization
- Manual test suite assembly

**Why Not Pure QuickCheck?**
- Better for properties than specific examples
- Less intuitive for testing specific scenarios
- Output less readable for specific test cases

**Why Not Tasty?**
- More flexible but requires more boilerplate
- Hspec's opinionated approach is simpler for our use case
- Tasty is great for complex test suites, overkill for us

**Decision:** Hspec's readability and BDD style make tests self-documenting, which is valuable for an educational project.

---

## Language Design Choices

### Static Typing Over Dynamic

**Why Static Types?**
- Catch errors at compile time (fail fast)
- Self-documenting code (types show intent)
- Better IDE support (autocomplete, refactoring)
- Zero runtime overhead for type checks

**Trade-off:** More verbose than dynamic typing, but worth it for safety

### No Implicit Type Conversions

**Why Strict Types?**
```c
// C allows dangerous conversions
int x = 3.7;      // Silent truncation
bool b = 42;      // Non-zero becomes true

// GLaDOS requires explicit types
int x = 3;        // Must be exact type
bool b = true;    // No confusion
```

**Benefits:**
- No silent data loss
- No confusion about intent
- Prevents entire class of bugs

**Trade-off:** More typing, but eliminates subtle bugs

### Stack-Based VM Over Native Code

**Why Bytecode VM?**

| Criterion | Stack VM | Register VM | Native Code | Our Choice |
|-----------|----------|-------------|-------------|------------|
| **Simplicity** | High | Moderate | Low | **Stack VM** |
| **Portability** | ✓ Perfect | ✓ Perfect | ✗ Per-platform | **Stack VM** |
| **Performance** | Good | Better | Best | **Stack VM** |
| **Debug-ability** | ✓ Easy | Moderate | Hard | **Stack VM** |
| **Implementation** | Simple | Complex | Very Complex | **Stack VM** |

**Key Benefits:**
1. **Simple to Implement**: Push/pop operations are intuitive
2. **Easy to Debug**: Can trace stack state at each instruction
3. **Portable**: Same bytecode runs anywhere
4. **Educational Value**: Clear execution model

**Why Not Native Code?**
- Requires architecture-specific code generation
- Complex register allocation
- Platform-specific debugging
- Harder to understand for learning

**Why Not Register VM?**
- More complex instruction encoding
- Register allocation adds complexity
- Marginal performance gain for our use case

**Decision:** Stack VM provides the best learning experience while maintaining reasonable performance.

---

## Project Structure Decisions

### Modular Directory Structure

```
src/
├── AST/          - Abstract Syntax Tree definitions
├── Parser/       - Megaparsec parser combinators
├── Security/     - Type checking and safety analysis
├── Compiler/     - IR generation and optimization
├── IR/           - Intermediate representation types
├── Bytecode/     - Binary serialization
├── Linker/       - Separate compilation support
├── VM/           - Virtual machine execution
└── Error/        - Error types and formatting
```

**Why This Structure?**
1. **Clear Separation of Concerns**: Each stage isolated
2. **Easy to Navigate**: Logical grouping by functionality
3. **Modular Testing**: Can test each stage independently
4. **Extensibility**: Easy to add new stages or features

**Alternative Considered:**
- Flat structure: All files in `src/`
- **Rejected:** Hard to navigate, unclear dependencies

### Security Module for Type Checking

**Why "Security" not "TypeChecker"?**
- Emphasizes that type safety is a security feature
- Includes type checking, initialization tracking, and control flow analysis
- Reflects philosophy: correctness IS security

**Benefits:**
- Makes security a first-class concern
- Encourages thinking about safety implications
- Aligns with modern secure coding practices

---

## Binary Serialization: Binary Library

### Why Haskell's Binary?

| Criterion | Binary | Cereal | Store | Custom | Our Choice |
|-----------|--------|--------|-------|--------|------------|
| **Simplicity** | High | High | Moderate | Low | **Binary** |
| **Performance** | Good | Good | Better | Varies | **Binary** |
| **Ecosystem** | Mature | Mature | Newer | N/A | **Binary** |
| **Type Safety** | ✓ Derive instances | ✓ Derive instances | ✓ Derive instances | Manual | **Binary** |

**Key Benefits:**
1. **Automatic Derivation**:
   ```haskell
   data Instruction = PushInt Int | AddInt | ...
     deriving (Generic, Binary)
   ```

2. **Type-Safe**: Compiler ensures correct serialization

3. **Lazy Evaluation**: Efficient for large files

4. **Standard Format**: Well-understood binary encoding

**Decision:** Binary provides the simplest solution with good performance and ecosystem support.

---

## Error Handling: Either vs Exceptions

### Why Either Types?

```haskell
parseProgram :: String -> Either String Program
typeCheck :: Program -> Either [TypeError] Program
compile :: Program -> Either CompilerError IRProgram
```

**Benefits:**
1. **Explicit in Type Signatures**: Function types show they can fail
2. **Composable with Monads**: Easy chaining with `do` notation or `>>=`
3. **Forced Error Handling**: Compiler ensures you handle errors
4. **Testable**: Easy to test error cases

**Why Not Exceptions?**
- Exceptions invisible in type signatures
- Can escape and crash program
- Harder to test systematically
- Less idiomatic in Haskell

**Decision:** Either types provide better type safety and explicit error handling, fitting Haskell's philosophy.

---

## Git Workflow

### Branch Strategy
- `main`: Stable releases
- `dev`: Development branch (we use this as our main working branch)
- Feature branches: For experimental features

**Why This Structure?**
- Clear separation between stable and development
- Easy to experiment without breaking main branch
- Standard practice in industry

---

## Future Considerations

### What We Might Change:

1. **Native Code Backend**: LLVM integration for performance
2. **Incremental Compilation**: Cache compiled modules
3. **Language Server Protocol**: IDE integration
4. **Optimizer**: Constant folding, dead code elimination

### What We'll Keep:

1. **Stack**: Excellent developer experience
2. **Megaparsec**: Flexibility and error messages
3. **Hspec**: Readable tests
4. **Static Types**: Safety guarantees

---

## Conclusion

Our technology choices prioritize:
1. **Learning**: Clear, understandable implementations
2. **Safety**: Type safety and error handling
3. **Productivity**: Good tooling and quick iteration
4. **Maintainability**: Modular structure and clear code

These decisions have served us well, providing a solid foundation for a safe, educational programming language implementation. While we could optimize for raw performance or minimalism, our choices reflect the project's goals: building a safe, understandable compiler that demonstrates modern language implementation techniques.
