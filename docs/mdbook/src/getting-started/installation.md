# Installation

This guide will help you set up GLaDOS on your system.

## Prerequisites

### System Requirements

- **Operating System**: Linux (Ubuntu/Debian recommended)
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

**For Ubuntu/Debian:**
```bash
sudo apt-get update
sudo apt-get install build-essential curl libffi-dev libffi8ubuntu1 \
    libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
```

**For CentOS/RHEL/Fedora:**
```bash
sudo dnf install gcc gcc-c++ gmp-devel make ncurses-devel zlib-devel
```

## Installation

### 1. Clone the Repository

```bash
git clone git@github.com:EpitechPGE3-2025/G-FUN-500-TLS-5-1-glados-2.git
cd glados
```

### 2. Build GLaDOS

**First time setup** (installs GHC and dependencies):
```bash
stack setup
stack build
```

**Build the compiler:**
```bash
# Using Makefile (recommended)
make

# Or using Stack directly
stack build --copy-bins --local-bin-path . && mv ./glados-exe ./glados
```

The compiled binary will be named `glados` in your project root.

### 3. Verify Installation

```bash
./glados --help
# Or test with a simple program
echo 'int main() { return 42; }' | ./glados
```

## Usage

GLaDOS supports multiple modes of operation:

```bash
# Compile and execute (default)
./glados < program.c

# Display Abstract Syntax Tree
./glados --ast < program.c

# Display human-readable IR
./glados --ir < program.c

# Compile to bytecode
./glados --compile < program.c > program.gbc

# Execute bytecode file
./glados --run program.gbc

# Show help
./glados --help
```

**Exit Codes:**
- `0` - Success
- `84` - Error (parse, type, or runtime error)

## Development Workflow

### Running Tests

```bash
# Run all tests
stack test

# Run with verbose output
stack test --test-arguments="--format=progress"

# Run with coverage
stack test --coverage
```

### Clean Build

```bash
make fclean && make
# Or
make re
```

## Next Steps

- Write [Your First Program](./first-program.md)
- Check out [Examples](./examples.md) for more programs
