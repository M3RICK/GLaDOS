# Team Members

This page recognizes the contributors to the GLaDOS project.

---

## Core Contributors

### Thierry B.
**Role:** Compiler & Virtual Machine

**Contributions:**
- Project architecture and design
- Complete compiler implementation
- Virtual machine design and execution engine
- Error handling and formatting system
- Build system setup and configuration
- Technical documentation

Thierry designed and implemented the core compilation pipeline, from IR generation to bytecode execution. He established the project structure and build tooling that enabled efficient team collaboration. He also implemented comprehensive error handling and formatting throughout the compiler, ensuring clear and helpful error messages for developers.

---

### Aymeric L.
**Role:** Parser, Security, Linker & Bonus Features

**Contributions:**
- Full modular parsing system
- Advanced security and type checking
- Linker system for separate compilation
- Bonus feature implementation
- Compiler refactoring and modularization

Aymeric built the complete parsing infrastructure using Megaparsec, implementing the full language grammar with proper error handling. He designed and implemented the comprehensive security layer including type checking, initialization tracking, and return path analysis. He also implemented the linker system enabling separate compilation and multi-module programs. Additionally, he refactored the compiler to eliminate illegal imports and improve modularity.

---

### Sven R.
**Role:** TBD

**Contributions:** TBD

---

### Tony F.
**Role:** TBD

**Contributions:** TBD

---

### Aurelien P.
**Role:** TBD

**Contributions:** TBD

---

## Project Statistics

- **Language:** Haskell (GHC 9.6.3)
- **Completion Percentage:** 99%
- **Test Coverage:** .c test files ready to be executed by .sh scripts + HSpec test files
- **Doc Status:** 100%
- **Compilation Stages:** 6 (Parsing → Type Checking → IR Generation → Bytecode Serialization → Linking → VM Execution)
- **Compiler Modes:** 9 (default, --ast, --ir, --compile, --compile-obj, --link, --run, --help, --version)

---

## Acknowledgments

This project was developed as part of the Epitech curriculum, demonstrating advanced concepts in:
- Functional programming with Haskell
- Compiler design and implementation
- Type systems and static analysis
- Virtual machine architecture
- Language security features
