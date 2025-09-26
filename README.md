<div align="center">

# GLaDOS

<img src="docs/assets/HolyC_Logo.png" alt="HolyC Logo" width="150"/>

**HolyC but Holyer — for the true zealots, the apostles of code.**

</div>

A functional programming language interpreter built in Haskell. This project implements a complete language pipeline from parsing to execution, starting with a LISP-like syntax and evolving into a custom language with advanced features.

## Technologies Used
- **Language**: Haskell (GHC 9.6.3)
- **Build System**: Stack (LTS 22.6)
- **Testing Framework**: Hspec
- **Parser Library**: Parsec
- **Resolver**: lts-22.6

## Prerequisites

Before setting up the project, ensure you have the following installed:

### System Requirements
- **Operating System**: Linux
- **Memory**: At least 4GB RAM (8GB recommended for compilation)
- **Disk Space**: ~2GB for Haskell toolchain and dependencies

### Required Dependencies

#### 1. Install Stack (Haskell Build Tool)

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

#### 2. Verify Installation
```bash
stack --version
# Should output something like: Version 2.13.1
```

#### 3. Install System Dependencies
```bash
# Ubuntu/Debian
sudo apt-get update
sudo apt-get install build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5

# CentOS/RHEL/Fedora
sudo dnf install gcc gcc-c++ gmp-devel make ncurses-devel zlib-devel
```

## Setup Instructions

### 1. Clone and Setup Project
```bash
# Clone the repository
git clone <your-repo-url>
cd glados

# Install GHC and dependencies (first time only)
stack setup

# Install project dependencies
stack build --dependencies-only
```

### 2. Build the Project
```bash
# Using Makefile (recommended)
make

# Or using Stack directly
stack build --copy-bins --local-bin-path . && mv ./glados-exe ./glados
```

### 3. Run Tests
```bash
# Run all tests
stack test

# Run tests with verbose output
stack test --test-arguments="--format=progress"

# Run with coverage
stack test --coverage
```

## Usage
```bash
# Build the project
make

# Run with file input
./glados < program.scm

# Interactive mode (planned)
./glados --repl

# Run specific example
./glados < examples/factorial.scm
```

## Project Structure
```
├── app/
│   └── Main.hs                # Entry point
├── src/
│   ├── Lib.hs                 # Core library
│   ├── Parser/                # Parsing modules
│   ├── Evaluator/             # Evaluation engine
│   └── VM/                    # Virtual machine (planned)
├── tests/
│   └── Spec.hs                # Test specifications
├── examples/                  # Example programs
├── docs/                      # Documentation
├── Makefile                   # Build configuration
├── package.yaml               # Project dependencies
└── stack.yaml                 # Stack configuration
```

## Team
| Name | Role | Contributions |
|------|------|---------------|
| Aymeric.L | Developer | Parser and Evaluator engine creation |
| Aurelien.P | Developer | [TBD] |
| Sven.R | Developer | [TBD] |
| Tony.F | Developer | [TBD] |
| Thierry.B | Developer | Build, Environment, Builtin and app structure |

## Build & Test Commands
```bash
# Clean build
make fclean && make

# Development cycle
make re

# Run tests
stack test

# Run tests with coverage
stack test --coverage
```

## Creating a Stack Project Yourself

If you want to create a similar Haskell project from scratch, follow these steps:

### 1. Initialize New Project
```bash
# Create project directory
mkdir my-haskell-project
cd my-haskell-project

# Initialize with Stack
stack new my-haskell-project --bare
# Or use a template: stack new my-haskell-project simple
```

### 2. Configure Project Files

**Create `package.yaml`:**
```yaml
name: my-haskell-project
version: 0.1.0.0
github: "yourusername/my-haskell-project"
author: "Your Name"
maintainer: "your.email@example.com"

dependencies:
- base >= 4.7 && < 5

library:
  source-dirs: src

executables:
  my-exe:
    main: Main.hs
    source-dirs: app
    dependencies:
    - my-haskell-project

tests:
  my-test:
    main: Spec.hs
    source-dirs: test
    dependencies:
    - my-haskell-project
    - hspec
```

**Create `stack.yaml`:**
```yaml
resolver: lts-22.6
packages:
- .
```

### 3. Create Directory Structure
```bash
mkdir -p src app test
touch src/Lib.hs app/Main.hs test/Spec.hs
```

### 4. Add Basic Code

**src/Lib.hs:**
```haskell
module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
```

**app/Main.hs:**
```haskell
module Main where
import Lib
main :: IO ()
main = someFunc
```

### 5. Build and Test
```bash
stack build
stack test
stack exec my-exe
```
