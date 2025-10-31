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
| **Reproducible Builds** | LTS snapshots | Partial (with freeze files) | Manual | **Stack** |
| **Dependency Management** | Automatic resolution | Manual constraints | None | **Stack** |
| **Ease of Setup** | One command | Moderate | Complex | **Stack** |
| **Project Isolation** | Separate environments | With sandboxes | Global | **Stack** |
| **Learning Curve** | Low | Moderate | High | **Stack** |

**Decision:** Stack provides the best developer experience for a team project with educational goals.

---

## Parser Library: Megaparsec

### What is Megaparsec?

Megaparsec is a modern parser combinator library for Haskell, successor to Parsec.

### Why Megaparsec?

| Criterion | Megaparsec | Parsec | Alex/Happy | Parser Generators | Our Choice |
|-----------|------------|--------|------------|-------------------|------------|
| **Error Messages** | Excellent | Good | Poor | Poor | **Megaparsec** |
| **Composability** | Combinator-based | Combinator-based | Separate tools | Generated code | **Megaparsec** |
| **Learning Curve** | Low | Low | Moderate | High | **Megaparsec** |
| **Flexibility** | High | High | Low | Low | **Megaparsec** |
| **Type Safety** | Strong | Strong | Moderate | Weak | **Megaparsec** |
| **Maintenance** | Active | Legacy | Active | Varies | **Megaparsec** |

1. **Built-in Lexer Support**: `Text.Megaparsec.Char.Lexer` handles whitespace and comments automatically

2. **Type-Safe**: Haskell's type system catches parser bugs at compile time

**Decision:** Megaparsec's composability and error messages make it ideal for educational purposes and rapid language iteration.

---

## Testing Framework: Hspec

### What is Hspec?

Hspec is a BDD (Behavior-Driven Development) testing framework for Haskell, inspired by Ruby's RSpec.

### Why Hspec?

| Criterion | Hspec | HUnit | QuickCheck | Tasty | Our Choice |
|-----------|-------|-------|------------|-------|------------|
| **Readability** | Descriptive | Moderate | Low | Moderate | **Hspec** |
| **BDD Style** | `describe`/`it` | Assertions | Properties | Mixed | **Hspec** |
| **Output Formatting** | Excellent | Basic | Basic | Good | **Hspec** |
| **Learning Curve** | Low | Low | Moderate | Moderate | **Hspec** |
| **Property Testing** | Integrates QuickCheck | No | Native | Yes | **Hspec** |
| **Parallel Execution** | Yes | No | No | Yes | **Hspec** |

**Decision:** Hspec's readability and BDD style make tests self-documenting, which is valuable for an educational project.

---

## Core Libraries

### Containers: Data Structures

#### What is Containers?

The `containers` library provides high-performance, immutable data structures for Haskell.

#### Why Containers?

| Criterion | containers | unordered-containers | vector | lists | Our Choice |
|-----------|------------|---------------------|---------|-------|------------|
| **Performance** | O(log n) | O(1) average | O(1) index | O(n) | **containers** |
| **Ordering** | Sorted | Unordered | Indexed | Ordered | **containers** |
| **Immutability** | Persistent | Persistent | Immutable | Immutable | **containers** |
| **Memory** | Efficient | Efficient | Very Efficient | Poor | **containers** |
| **Pattern Matching** | Good | Moderate | Limited | Excellent | **containers** |

**Our Usage:**
- `Map`: Symbol tables, type environments, function tables
- `Set`: Tracking initialized variables, imported modules
- `Seq`: Instruction sequences, statement lists

**Decision:** Containers provides the perfect balance of performance and immutability for compiler data structures.

---

## Binary Data Handling

### ByteString: Binary Data Handling

#### What is ByteString?

Efficient, immutable byte array representation for binary data.

#### Why ByteString?

| Criterion | ByteString | [Word8] | Vector Word8 | Our Choice |
|-----------|------------|---------|--------------|------------|
| **Performance** | Excellent | Poor | Excellent | **ByteString** |
| **Memory** | Packed | Wasteful | Packed | **ByteString** |
| **I/O** | Optimized | Slow | Manual | **ByteString** |
| **Binary Integration** | Native | Manual | Manual | **ByteString** |
| **Ecosystem** | Mature | N/A | Growing | **ByteString** |

**Key Benefits:**

**Our Usage:**
- `.glc` bytecode files
- `.glo` object files
- Binary serialization/deserialization
- File I/O operations

**Decision:** ByteString is the standard, battle-tested solution for binary data in Haskell.

---

## Evaluation Control: DeepSeq

### What is DeepSeq?

Library for controlling evaluation strictness in lazy Haskell.

### Why DeepSeq?

| Criterion | DeepSeq | seq | BangPatterns | Our Choice |
|-----------|---------|-----|--------------|------------|
| **Deep Evaluation** | Recursive | Shallow | Shallow | **DeepSeq** |
| **Type Safety** | NFData class | No | No | **DeepSeq** |
| **Composability** | Derives | Manual | Manual | **DeepSeq** |
| **Control** | Explicit | Explicit | Implicit | **DeepSeq** |

**Key Benefits:**

**Our Usage:**
- Force evaluation before writing bytecode
- Prevent space leaks in compilation
- Ensure error messages are fully evaluated
- Accurate benchmarking

**Decision:** DeepSeq prevents space leaks and ensures predictable memory behavior in our compiler.

---

## Testing Infrastructure

### Hspec-Discover: Automatic Test Discovery

#### What is Hspec-Discover?

Automatic test discovery tool for Hspec - finds all test files and generates test suite automatically.

#### Why Hspec-Discover?

| Criterion | hspec-discover | Manual main | Tasty-discover | Our Choice |
|-----------|----------------|-------------|----------------|------------|
| **Automation** | Fully automatic | Manual | Automatic | **hspec-discover** |
| **Convention** | `*Spec.hs` files | Custom | Custom | **hspec-discover** |
| **Maintenance** | Zero | High | Low | **hspec-discover** |
| **Integration** | Native Hspec | N/A | Different API | **hspec-discover** |

**Key Benefits:**

**Decision:** Hspec-discover eliminates boilerplate and ensures we never forget to run a test.

---

### Process: External Program Execution

#### What is Process?

Library for creating and interacting with system processes.

#### Why Process?

| Criterion | process | System.Cmd | ShellCmd | Raw fork | Our Choice |
|-----------|---------|------------|----------|----------|------------|
| **Type Safety** | Strong | Weak | Moderate | Unsafe | **process** |
| **Error Handling** | Explicit | Weak | Moderate | Manual | **process** |
| **Portability** | Cross-platform | Limited | Limited | Unix only | **process** |
| **Control** | High | Low | Moderate | Total | **process** |
| **Output Capture** | Easy | Manual | Easy | Manual | **process** |

**Our Usage:**
- Run compiled programs in tests
- Test linker by calling glados-exe
- Integration tests for full pipeline
- Verify error messages from command-line tool

**Decision:** Process provides safe, portable process execution for comprehensive integration testing.

---

### QuickCheck: Property-Based Testing

#### What is QuickCheck?

Property-based testing library that generates random test cases to verify properties.

#### Why QuickCheck?

| Criterion | QuickCheck | Example-Based | SmallCheck | Hedgehog | Our Choice |
|-----------|------------|---------------|------------|----------|------------|
| **Coverage** | Excellent | Manual | Complete (small) | Excellent | **QuickCheck** |
| **Random Testing** | Yes | No | Exhaustive | Yes | **QuickCheck** |
| **Shrinking** | Automatic | N/A | N/A | Better | **QuickCheck** |
| **Integration** | Hspec native | Yes | Yes | Manual | **QuickCheck** |
| **Maturity** | Very mature | N/A | Mature | Newer | **QuickCheck** |

**Our Usage:**
- Test compiler properties (type preservation)
- Verify VM correctness (semantics preservation)
- Test parser round-tripping (parse . pretty = id)
- Find edge cases in type checker

**Decision:** QuickCheck finds bugs that example-based tests miss, and integrates perfectly with our Hspec test suite.

---

## Parser Combinators

### Parser-Combinators: Megaparsec Utilities

#### What is Parser-Combinators?

Companion library to Megaparsec providing lightweight, general parser combinators.

#### Why Parser-Combinators?

**Key Benefits:**

1. **Separation of Concerns**:
   - Megaparsec: Core parsing functionality
   - Parser-combinators: Reusable combinators
   - Keeps Megaparsec lightweight

2. **Useful Combinators**:
   ```haskell
   import Control.Monad.Combinators

   -- Many with separator
   paramList = param `sepBy` comma

   -- Between delimiters
   block = between (symbol "{") (symbol "}") statements

   -- Optional with default
   optionalSemicolon = option () semicolon
   ```

**Our Usage:**
- `sepBy`, `sepBy1`: Comma-separated lists
- `between`: Parentheses, braces, brackets
- `choice`: Multiple alternatives
- `option`: Optional syntax elements

**Decision:** Parser-combinators provides standard combinators that keep our parser code clean and readable.

## Binary Serialization: Binary Library

### Why Haskell's Binary?

| Criterion | Binary | Cereal | Store | Custom | Our Choice |
|-----------|--------|--------|-------|--------|------------|
| **Simplicity** | High | High | Moderate | Low | **Binary** |
| **Performance** | Good | Good | Better | Varies | **Binary** |
| **Ecosystem** | Mature | Mature | Newer | N/A | **Binary** |
| **Type Safety** | Derive instances | Derive instances | Derive instances | Manual | **Binary** |

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
- `dev_(feature)`Feature branches: For experimental features

**Why This Structure?**
- Clear separation between stable and development
- Easy to experiment without breaking main branch
- Standard practice in industry

---

## Compiler Configuration

### GHC Warning Flags

We enable extensive warning flags to catch bugs early and maintain code quality.

#### Enabled Warnings

```yaml
ghc-options:
  - -Wall
  - -Wcompat
  - -Widentities
  - -Wincomplete-record-updates
  - -Wincomplete-uni-patterns
  - -Wmissing-export-lists
  - -Wmissing-home-modules
  - -Wpartial-fields
  - -Wredundant-constraints
```

#### Why Each Warning?

**-Wall (Enable All Standard Warnings)**
- Catches unused variables, imports, and patterns
- Detects type defaulting and orphan instances
- Essential baseline for code quality

**-Wcompat (Future Compatibility)**
- Warns about upcoming breaking changes
- Helps with GHC version migration
- Prevents reliance on deprecated features

**-Wincomplete-record-updates**
```haskell
updateName record = record { name = "new" }

-- GOOD: Pattern match ensures field exists
updateName record@Record{..} = record { name = "new" }
```

**-Wincomplete-uni-patterns**
```haskell
-- BAD: Pattern match can fail
let Just x = maybeValue in x + 1

-- GOOD: Exhaustive pattern match
case maybeValue of
  Just x -> x + 1
  Nothing -> 0
```

**-Wmissing-export-lists**
```haskell
-- BAD: Everything exported
module Parser where

-- GOOD: Explicit exports
module Parser (parseProgram, ParseError) where
```
- Prevents accidental API exposure
- Makes module interface explicit
- Helps maintain encapsulation

**-Wpartial-fields**
```haskell
-- BAD: Partial record
data Point = Point2D { getX :: Int, getY :: Int }
           | Point3D { getX :: Int, getZ :: Int }

-- GOOD: Total fields
data Point = Point2D Int Int | Point3D Int Int Int
```

**-Wredundant-constraints**
```haskell
-- BAD: Ord not needed
sort :: (Ord a, Show a) => [a] -> [a]

-- GOOD: Only necessary constraints
sort :: Ord a => [a] -> [a]
```

#### Runtime Options

```yaml
ghc-options:
  - -threaded
  - -rtsopts
  - -with-rtsopts=-N
```

**Decision:** Strict warnings catch bugs at compile time, and runtime options enable performance optimization without code changes.

---

## Base Library and Language Extensions

### Base: Haskell Standard Library

**Version Constraint:** `base >= 4.7 && < 5`

**Decision:** Base library is required for any Haskell program; our constraint ensures stability while allowing modern GHC features.

---

## Library Summary

### Complete Dependency List

Here's a quick reference of all libraries used in GLaDOS:

| Library | Purpose | Why Not Alternative? |
|---------|---------|---------------------|
| **base** | Standard library | Required foundation |
| **megaparsec** | Parser combinators | Better errors than Parsec, more flexible than generators |
| **parser-combinators** | Parser utilities | Standard combinators for megaparsec |
| **containers** | Map, Set, Seq | Deterministic ordering, efficient immutable operations |
| **bytestring** | Binary data | Standard for binary I/O |
| **binary** | Serialization | Simple automatic derivation |
| **deepseq** | Force evaluation | Prevents space leaks |
| **hspec** | BDD testing | Readable test output |
| **hspec-discover** | Test discovery | Zero-maintenance test suite |
| **QuickCheck** | Property testing | Finds edge cases automatically |
| **process** | External programs | End-to-end integration tests |

### Dependency Graph

```
Application Layer:
├── Parser (megaparsec, parser-combinators)
├── Type Checker (containers)
├── Compiler (containers, bytestring)
├── VM (bytestring, binary, deepseq)
└── Tests (hspec, hspec-discover, QuickCheck, process)

Infrastructure:
├── Build System (Stack)
└── Compiler (GHC with strict warnings)
```
---

## Conclusion

Our technology choices prioritize:
1. **Learning**: Clear, understandable implementations
2. **Safety**: Type safety and error handling
3. **Productivity**: Good tooling and quick iteration
4. **Maintainability**: Modular structure and clear code
