<div align="center">

# GLaDOS

<img src="docs/assets/HolyC_Logo.png" alt="HolyC Logo" width="150"/>

**HolyC but Holyer — for the true zealots, the apostles of code.**

</div>

---

## Full Documentation

If you are interested in the full documentation of this project, do not hesitate to check out the true documentation here:

**[https://crackedontiti.github.io/Glados_Documentation/](https://crackedontiti.github.io/Glados_Documentation/)**

**Project Management & Task Tracking:**
[Notion - GLaDOS Project Board](https://www.notion.so/GLADOS-2726219a901480379f0cc6d6814ca0f9?source=copy_link)

---

## Quick Start

### Prerequisites
```bash
# Install Stack
curl -sSL https://get.haskellstack.org/ | sh

# Install system dependencies (Ubuntu/Debian)
sudo apt-get update
sudo apt-get install build-essential curl libffi-dev libgmp-dev libncurses-dev
```

### Build
```bash
git clone git@github.com:EpitechPGE3-2025/G-FUN-500-TLS-5-1-glados-2.git
cd glados
make
```

### Run
```bash
# Compile and execute a GLaDOS program (default)
./glados < program.c

# Show Abstract Syntax Tree
./glados --ast < program.c

# Show human-readable IR
./glados --ir < program.c

# Compile to bytecode
./glados --compile < program.c > program.gbc

# Execute bytecode
./glados --run program.gbc
```

---

## Quick Example

**hello.c:**
```c
int main() {
    int x = 5
    int y = 10
    return x + y
}
```

**Run it:**
```bash
./glados < hello.c
# Output: VInt 15
```

**Note:** Semicolons are optional (HolyC style). Use them, don't use them, mix them - GLaDOS doesn't judge.

---

## Key Features

- **C-like syntax** inspired by HolyC
- **Custom VM** with WebAssembly-like bytecode
- **IR output** for debugging (human-readable disassembly)
- **Bytecode compilation** for efficient execution
- **Optional semicolons** (following King Terry Davis)
- **Static typing** with int and bool types
- **Recursion** and control flow support

---

## Compiler Flags

| Mode | Description |
|------|-------------|
| (default) | Read from stdin, compile and execute |
| `--ast` | Parse and display the Abstract Syntax Tree |
| `--ir` | Parse, compile and display human-readable IR |
| `--compile` | Compile to bytecode and write to stdout |
| `--run FILE` | Execute bytecode from FILE |
| `--help` | Display this help message |

**Exit Codes:**
- `0` - Success
- `84` - Error (parse, type, or runtime error)

---

## Testing

```bash
# Run all tests
stack test

# Run with coverage
stack test --coverage
```

---

## Team

| Name | Role |
|------|------|
| **Aymeric L.** | Parser & Security |
| **Thierry B.** | Compiler & VM |
| **Aurelien P.** | TBD |
| **Sven R.** | TBD |
| **Tony F.** | TBD |

---

## Project Info

- **Language:** Haskell (GHC 9.6.3)
- **Build Tool:** Stack
- **Completion:** 98%
- **Documentation:** [crackedontiti.github.io/Glados_Documentation](https://crackedontiti.github.io/Glados_Documentation/)

---

## Bonus Modules

### `Pointer.hs` - Security Checker

Prevents unsafe pointer-like operations (e.g. `*`, `&`, `ptr`, `ref`) in user code.
If detected, it raises:  
`SecurityError: pointer manipulation is not allowed in glados.`

```haskell
> checkPointers "(define x 42)"
Right ()
> checkPointers "(define ptrX (* &x))"
Left "SecurityError..."
```

### `Outputs.hs` - Output Manager

Handles all program outputs:  
- `Print` → standard output (`stdout`)  
- `Printerror` → error output (`stderr`)  
- `Return` → return value of the program

```haskell
exec [ Print (VInt 42)
     , Printerror (VString "Erreur !")
     , Return (VBool True) ]
```

---

## For Everything Else

**RTFM:** [Full Documentation](https://crackedontiti.github.io/Glados_Documentation/)

This README is intentionally brief. For comprehensive guides on syntax, compilation pipeline, security features, and more, visit the full documentation.

---

*"Code the way Terry intended: free."*
