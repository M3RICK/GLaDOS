User Input (source code)
    ↓
parseProgram (Parser/Core.hs)
    ↓
Program AST
    ↓
checkProgram (Security/TypeChecker.hs)
    ├→ collectFunctionSignatures (builds function environment)
    ├→ checkFunction (for each function)
    │   ├→ makeFunctionEnv (sets up variable environment)
    │   ├→ checkStatements (validates function body)
    │   │   └→ checkStatement (per statement)
    │   │       ├→ checkExpr (validates expressions)
    │   │       │   ├→ checkBinOp (operators)
    │   │       │   │   └→ checkNotZero (div by zero)
    │   │       │   └→ validateInitialized (uninitialized vars)
    │   │       └→ [other checks...]
    │   └→ listHasReturn (return path checking)
    │
    └→ Returns: Either [TypeError] Program
