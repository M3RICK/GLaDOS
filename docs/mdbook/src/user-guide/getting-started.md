# Getting Started

This guide will help you set up GLaDOS on your system and write your first program.

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
git clone <your-repo-url>
cd glados
```

### 2. Build GLaDOS

**First time setup** (installs GHC and dependencies):
```bash
stack setup
stack build --dependencies-only
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
./glados --version  # (if you added version flag)
# Or test with a simple program
echo 'int main() { return 42; }' | ./glados
```

## Your First Program

Create a file called `hello.c`:

```c
int main() {
    int x = 5;
    int y = 10;
    int result = x + y;
    return result;
}
```

### Compile and Run

```bash
./glados < hello.c
```

**Expected output:**
```
15
```

## Quick Examples

### Factorial

```c
int factorial(int n) {
    if (n <= 1) {
        return 1;
    }
    return n * factorial(n - 1);
}

int main() {
    int result = factorial(5);
    return result;
}
```

Output: `120`

### Fibonacci

```c
int fib(int n) {
    if (n <= 1) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

int main() {
    return fib(10);
}
```

Output: `55`

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

- Read the [Syntax](./syntax.md) guide to learn the language
- Check out [Examples](./examples.md) for more programs
- Learn about [Error Messages](./error-messages.md) to debug your code
