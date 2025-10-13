# VM Execution

This page details the GLaDOS Virtual Machine (VM), which executes the compiled IR bytecode using a stack-based architecture.

## Overview

The VM is implemented in `src/VM.hs` and provides a **stack machine** that executes IR instructions sequentially, maintaining execution state through stacks and environments.

## VM Architecture

### VM State Components

```haskell
data VMState = VMState
  { instructions :: [Instruction]    -- Program instructions
  , pc :: Int                        -- Program counter (current instruction)
  , valueStack :: [Value]            -- Value stack
  , callStack :: [CallFrame]         -- Function call frames
  , environment :: Environment       -- Variable bindings
  }

type Environment = Map String Value

data CallFrame = CallFrame
  { returnPC :: Int                  -- Return address
  , savedEnv :: Environment          -- Saved environment
  }
```

### Stack Machine Model

The VM operates as a **stack machine**:

1. **Value Stack**: Holds intermediate values during computation
2. **Call Stack**: Tracks function calls and return addresses
3. **Environment**: Maps variable names to values
4. **Program Counter**: Points to current instruction

## Instruction Execution

### Execution Loop

```haskell
execute :: VMState -> Either String Value
execute state
  | pc state >= length (instructions state) =
      -- End of program
      case valueStack state of
        (result:_) -> Right result
        [] -> Left "Runtime error: Empty stack at program end"

  | otherwise =
      let inst = instructions state !! pc state
      in case executeInstruction inst state of
           Left err -> Left err
           Right newState -> execute newState

executeInstruction :: Instruction -> VMState -> Either String VMState
executeInstruction inst state = case inst of
  Push val -> ...
  Pop -> ...
  Load var -> ...
  Store var -> ...
  Add -> ...
  -- ... etc
```

### Stack Operations

**Push**
```haskell
executeInstruction (Push val) state =
  Right state
    { valueStack = val : valueStack state
    , pc = pc state + 1
    }
```

**Pop**
```haskell
executeInstruction Pop state =
  case valueStack state of
    (_:rest) -> Right state
      { valueStack = rest
      , pc = pc state + 1
      }
    [] -> Left "Runtime error: Stack underflow"
```

**PopN**
```haskell
executeInstruction (PopN n) state =
  if length (valueStack state) >= n
    then Right state
      { valueStack = drop n (valueStack state)
      , pc = pc state + 1
      }
    else Left "Runtime error: Stack underflow"
```

### Variable Operations

**Load**
```haskell
executeInstruction (Load varName) state =
  case Map.lookup varName (environment state) of
    Just val -> Right state
      { valueStack = val : valueStack state
      , pc = pc state + 1
      }
    Nothing -> Left $ "Runtime error: Undefined variable '" ++ varName ++ "'"
```

**Store**
```haskell
executeInstruction (Store varName) state =
  case valueStack state of
    (val:rest) -> Right state
      { valueStack = rest
      , environment = Map.insert varName val (environment state)
      , pc = pc state + 1
      }
    [] -> Left "Runtime error: Stack underflow"
```

### Arithmetic Operations

All arithmetic operations follow the same pattern:

```haskell
executeInstruction Add state =
  case valueStack state of
    (IntVal b : IntVal a : rest) -> Right state
      { valueStack = IntVal (a + b) : rest
      , pc = pc state + 1
      }
    _ -> Left "Runtime error: Type error in addition"

executeInstruction Sub state =
  case valueStack state of
    (IntVal b : IntVal a : rest) -> Right state
      { valueStack = IntVal (a - b) : rest
      , pc = pc state + 1
      }
    _ -> Left "Runtime error: Type error in subtraction"

executeInstruction Mul state =
  case valueStack state of
    (IntVal b : IntVal a : rest) -> Right state
      { valueStack = IntVal (a * b) : rest
      , pc = pc state + 1
      }
    _ -> Left "Runtime error: Type error in multiplication"

executeInstruction Div state =
  case valueStack state of
    (IntVal b : IntVal a : rest) ->
      if b == 0
        then Left "Runtime error: Division by zero"
        else Right state
          { valueStack = IntVal (a `div` b) : rest
          , pc = pc state + 1
          }
    _ -> Left "Runtime error: Type error in division"
```

**Key Points:**
- Pop operands in reverse order (b then a for a op b)
- Check types at runtime
- Division checks for zero at runtime
- Push result back onto stack

### Comparison Operations

```haskell
executeInstruction Eq state =
  case valueStack state of
    (b : a : rest) -> Right state
      { valueStack = BoolVal (a == b) : rest
      , pc = pc state + 1
      }
    _ -> Left "Runtime error: Stack underflow"

executeInstruction Lt state =
  case valueStack state of
    (IntVal b : IntVal a : rest) -> Right state
      { valueStack = BoolVal (a < b) : rest
      , pc = pc state + 1
      }
    _ -> Left "Runtime error: Type error in comparison"

-- Similar for Neq, Gt, Lte, Gte
```

### Logical Operations

```haskell
executeInstruction And state =
  case valueStack state of
    (BoolVal b : BoolVal a : rest) -> Right state
      { valueStack = BoolVal (a && b) : rest
      , pc = pc state + 1
      }
    _ -> Left "Runtime error: Type error in logical AND"

executeInstruction Or state =
  case valueStack state of
    (BoolVal b : BoolVal a : rest) -> Right state
      { valueStack = BoolVal (a || b) : rest
      , pc = pc state + 1
      }
    _ -> Left "Runtime error: Type error in logical OR"
```

### Control Flow Operations

**Label**
```haskell
executeInstruction (Label _) state =
  -- Labels are no-ops at runtime, just move to next instruction
  Right state { pc = pc state + 1 }
```

**Jump**
```haskell
executeInstruction (Jump label) state =
  case findLabel label (instructions state) of
    Just targetPC -> Right state { pc = targetPC }
    Nothing -> Left $ "Runtime error: Unknown label '" ++ label ++ "'"

findLabel :: String -> [Instruction] -> Maybe Int
findLabel label instrs =
  findIndex (\inst -> inst == Label label) instrs
```

**JumpIfFalse**
```haskell
executeInstruction (JumpIfFalse label) state =
  case valueStack state of
    (BoolVal False : rest) ->
      case findLabel label (instructions state) of
        Just targetPC -> Right state
          { valueStack = rest
          , pc = targetPC
          }
        Nothing -> Left $ "Runtime error: Unknown label '" ++ label ++ "'"

    (BoolVal True : rest) ->
      Right state
        { valueStack = rest
        , pc = pc state + 1
        }

    _ -> Left "Runtime error: Type error in conditional jump"
```

### Function Operations

**Call**
```haskell
executeInstruction (Call funcName arity) state =
  -- Find function label
  case findLabel funcName (instructions state) of
    Nothing -> Left $ "Runtime error: Undefined function '" ++ funcName ++ "'"
    Just funcPC ->
      -- Pop arguments from stack
      let (args, restStack) = splitAt arity (valueStack state)
      in if length args < arity
           then Left "Runtime error: Not enough arguments on stack"
           else
             -- Create new call frame
             let frame = CallFrame
                   { returnPC = pc state + 1
                   , savedEnv = environment state
                   }
                 -- Bind parameters to arguments
                 newEnv = bindParameters funcName arity args (environment state)
             in Right state
               { valueStack = restStack
               , callStack = frame : callStack state
               , environment = newEnv
               , pc = funcPC
               }

bindParameters :: String -> Int -> [Value] -> Environment -> Environment
bindParameters funcName arity args env =
  -- Look up parameter names from function definition
  -- Bind args to parameters in environment
  -- (Simplified - actual implementation uses function metadata)
  foldl (\e (name, val) -> Map.insert name val e) env (zip paramNames args)
```

**Return**
```haskell
executeInstruction Return state =
  case valueStack state of
    [] -> Left "Runtime error: No return value on stack"
    (returnVal:_) ->
      case callStack state of
        [] ->
          -- Top-level return, end execution
          Right state  -- Will be caught by execute loop

        (frame:restFrames) ->
          -- Return from function call
          Right state
            { valueStack = returnVal : valueStack state
            , callStack = restFrames
            , environment = savedEnv frame
            , pc = returnPC frame
            }
```

## Execution Examples

### Example 1: Simple Arithmetic

**IR:**
```
Push 5
Push 3
Add
Return
```

**Execution trace:**
```
PC=0: Push 5          Stack: [5]              Env: {}
PC=1: Push 3          Stack: [3, 5]           Env: {}
PC=2: Add             Stack: [8]              Env: {}
PC=3: Return          Result: 8
```

### Example 2: Variable Operations

**IR:**
```
Push 42
Store "x"
Load "x"
Push 1
Add
Return
```

**Execution trace:**
```
PC=0: Push 42         Stack: [42]             Env: {}
PC=1: Store "x"       Stack: []               Env: {x=42}
PC=2: Load "x"        Stack: [42]             Env: {x=42}
PC=3: Push 1          Stack: [1, 42]          Env: {x=42}
PC=4: Add             Stack: [43]             Env: {x=42}
PC=5: Return          Result: 43
```

### Example 3: Conditional Jump

**IR:**
```
Push 5
Push 10
Lt
JumpIfFalse else_1
  Push 1
  Return
Label else_1
  Push 0
  Return
```

**Execution trace:**
```
PC=0: Push 5          Stack: [5]              Env: {}
PC=1: Push 10         Stack: [10, 5]          Env: {}
PC=2: Lt              Stack: [True]           Env: {}
PC=3: JumpIfFalse     Stack: []               Env: {}  (condition True, continue)
PC=4: Push 1          Stack: [1]              Env: {}
PC=5: Return          Result: 1
```

### Example 4: Function Call

**IR:**
```
Label "double"
  Load "x"
  Push 2
  Mul
  Return

Label "main"
  Push 5
  Call "double" 1
  Return
```

**Execution trace:**
```
PC=6: Push 5          Stack: [5]              Env: {}          Calls: []
PC=7: Call "double"   Stack: []               Env: {x=5}       Calls: [Frame(PC=8)]
PC=1: Load "x"        Stack: [5]              Env: {x=5}       Calls: [Frame(PC=8)]
PC=2: Push 2          Stack: [2, 5]           Env: {x=5}       Calls: [Frame(PC=8)]
PC=3: Mul             Stack: [10]             Env: {x=5}       Calls: [Frame(PC=8)]
PC=4: Return          Stack: [10]             Env: {}          Calls: []
PC=8: Return          Result: 10
```

## Runtime Error Handling

The VM detects various runtime errors:

### Stack Errors

```haskell
-- Stack underflow
Pop with empty stack -> "Runtime error: Stack underflow"
Add with <2 values   -> "Runtime error: Stack underflow"

-- Wrong value count
PopN 5 with 3 values -> "Runtime error: Stack underflow"
```

### Type Errors

```haskell
-- Type mismatch
Add with BoolVal     -> "Runtime error: Type error in addition"
Div with BoolVal     -> "Runtime error: Type error in division"
And with IntVal      -> "Runtime error: Type error in logical AND"
```

### Division Errors

```haskell
-- Division by zero
Div with divisor 0   -> "Runtime error: Division by zero"
```

### Variable Errors

```haskell
-- Undefined variable
Load "x" with x not in env -> "Runtime error: Undefined variable 'x'"
```

### Control Flow Errors

```haskell
-- Unknown label
Jump "foo" when foo doesn't exist -> "Runtime error: Unknown label 'foo'"
```

### Function Errors

```haskell
-- Undefined function
Call "foo" when foo doesn't exist -> "Runtime error: Undefined function 'foo'"

-- Missing arguments
Call with arity > stack size -> "Runtime error: Not enough arguments on stack"

-- Missing return value
Return with empty stack -> "Runtime error: No return value on stack"
```

## VM Performance

### Time Complexity

- **Instruction dispatch**: O(1) per instruction
- **Stack operations**: O(1) push/pop
- **Variable lookup**: O(log n) where n is environment size
- **Label lookup**: O(n) where n is program size (could be optimized with preprocessing)
- **Overall**: O(m × n) where m is instructions executed, n is program size

### Space Complexity

- **Value stack**: O(d) where d is maximum stack depth
- **Call stack**: O(c) where c is maximum call depth
- **Environment**: O(v) where v is number of variables in scope
- **Overall**: O(d + c + v)

### Optimization Opportunities

1. **Label preprocessing**: Build label→PC map before execution (O(1) jumps)
2. **Environment optimization**: Use arrays for local variables
3. **Instruction caching**: Cache decoded instructions
4. **JIT compilation**: Compile hot paths to native code

Currently, GLaDOS uses **no optimizations** - the VM is a simple interpreter.

## Tail Call Optimization

GLaDOS does **not** perform tail call optimization. Recursive functions can cause stack overflow:

```c
// This will eventually overflow the call stack
int infinite(int x) {
    return infinite(x + 1);
}
```

Tail call optimization could be added by detecting tail position returns and reusing call frames.

## Debugging Support

### Execution Tracing

The VM can be instrumented for debugging:

```haskell
executeWithTrace :: VMState -> IO (Either String Value)
executeWithTrace state = do
  putStrLn $ "PC=" ++ show (pc state) ++
             " Stack=" ++ show (valueStack state) ++
             " Env=" ++ show (environment state)
  -- Execute instruction
  case executeInstruction (instructions state !! pc state) state of
    Left err -> return $ Left err
    Right newState -> executeWithTrace newState
```

### Breakpoints

Breakpoints can be implemented by checking PC:

```haskell
executeWithBreakpoints :: [Int] -> VMState -> Either String Value
executeWithBreakpoints breakpoints state
  | pc state `elem` breakpoints =
      -- Pause and allow inspection
      inspectState state >> execute state
  | otherwise =
      execute state
```

## Testing the VM

### Unit Tests

```haskell
-- Test: Arithmetic
testArithmetic :: Spec
testArithmetic = it "executes 5 + 3" $ do
  let program = [Push (IntVal 5), Push (IntVal 3), Add, Return]
  let result = execute (initState program)
  result `shouldBe` Right (IntVal 8)

-- Test: Variables
testVariables :: Spec
testVariables = it "stores and loads variables" $ do
  let program = [Push (IntVal 42), Store "x", Load "x", Return]
  let result = execute (initState program)
  result `shouldBe` Right (IntVal 42)

-- Test: Function call
testFunctionCall :: Spec
testFunctionCall = it "calls functions correctly" $ do
  let program =
        [ Label "double"
        , Load "x", Push (IntVal 2), Mul, Return
        , Label "main"
        , Push (IntVal 5), Call "double" 1, Return
        ]
  let result = execute (initState program)
  result `shouldBe` Right (IntVal 10)
```

### Integration Tests

```bash
# Run all VM tests
stack test --test-arguments="--match VM"

# Test with actual GLaDOS programs
./glados < tests/dotC/arithmetic.c
./glados < tests/dotC/factorial.c
./glados < tests/dotC/fibonacci.c
```

## VM Extensions

Potential future extensions:

1. **Garbage collection**: For heap-allocated data structures
2. **Foreign function interface**: Call external C functions
3. **Concurrency**: Support for threads and synchronization
4. **Debugging protocol**: Remote debugging support
5. **Profiling**: Performance analysis tools

## Comparison with Other VMs

### Stack vs Register VMs

**GLaDOS (Stack-based)**:
- Simpler instruction set
- Easier to generate code for
- More instructions for same computation
- Examples: JVM, Python bytecode

**Register-based**:
- Fewer instructions
- More complex instruction set
- Requires register allocation
- Examples: Lua VM, LLVM

### Why Stack-Based?

1. **Simplicity**: Easier to implement and understand
2. **Code generation**: Straightforward AST→IR translation
3. **Portability**: No assumptions about target architecture
4. **Correctness**: Easier to verify correctness

## Summary

The GLaDOS VM provides:

- **Simple execution model**: Stack-based with clear semantics
- **Error detection**: Runtime type and bounds checking
- **Function support**: First-class functions with proper call frames
- **Debugging**: Can be instrumented for tracing
- **Correctness**: Matches language semantics precisely

Next steps:
- [Compilation Overview](./overview.md): Complete compilation pipeline
- [IR Generation](./ir-generation.md): How IR is produced
- [Security Features](../security/features.md): Safety guarantees
