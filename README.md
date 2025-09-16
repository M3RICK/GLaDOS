# GLADOS
**Generic Language and Data Operand Syntax**

A functional programming language interpreter built in Haskell. This project implements a complete language pipeline from parsing to execution, starting with a LISP-like syntax and evolving into a custom language with advanced features.

## Technologies Used

- **Language**: Haskell (GHC 9.6.3)
- **Build System**: Stack (LTS 22.6)
- **Resolver**: lts-22.6

## Usage

```bash
# Build the project
make

# Run with file input
./glados < program.scm

# Interactive mode (planned)
./glados --repl
```

## Project Structure

```
├── app/
│   └── Main.hs           # Entry point
├── src/
│   ├── Lib.hs           # Core library
│   ├── Parser/          # Parsing modules
│   ├── Evaluator/       # Evaluation engine
│   └── VM/              # Virtual machine (planned)
├── test/
│   └── Spec.hs          # Test specifications
├── examples/            # Example programs
├── docs/                # Documentation
└── Makefile             # Build configuration
```

## Team

| Name | Role | Contributions |
|------|------|---------------|
| Aymeric.L | Developer | [TBD] |
| Aurelien.P | Developer | [TBD] |
| Sven.R | Developer | [TBD] |
| Thierry.B | Developer | Build setup and main Project structure |

## Build & Test

```bash
# Clean build
make fclean && make

# Run tests
stack test

# Generate documentation
stack haddock
```