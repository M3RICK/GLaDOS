# Parsing

This page details the GLaDOS parser implementation, which transforms source code into an Abstract Syntax Tree (AST) using Megaparsec parser combinators.

## Overview

The parser is implemented in `src/Parser.hs` and consists of two main phases:

1. **Lexical Analysis (Lexing)**: Convert source code into tokens
2. **Syntactic Analysis (Parsing)**: Build AST from tokens

Both phases are implemented using **Megaparsec**, a powerful parser combinator library for Haskell.

## Lexical Analysis

### Token Types

The lexer recognizes several categories of tokens:

**Keywords:**
```haskell
keywords = ["int", "bool", "if", "else", "while", "return", "true", "false"]
```

**Operators:**
```haskell
operators =
  [ "+", "-", "*", "/"        -- Arithmetic
  , "==", "!=", "<", ">", "<=", ">="  -- Comparison
  , "&&", "||"                 -- Logical
  , "="                        -- Assignment
  ]
```

**Delimiters:**
```haskell
delimiters = ["(", ")", "{", "}", ";", ","]
```

**Identifiers:**
```
<identifier> ::= [a-zA-Z_][a-zA-Z0-9_]*
```

**Integer Literals:**
```
<integer> ::= [0-9]+
```

**Boolean Literals:**
```
<boolean> ::= "true" | "false"
```

### Whitespace and Comments

The lexer automatically handles:

- **Whitespace**: Spaces, tabs, newlines
- **Line comments**: `// comment text`
- **Block comments**: `/* comment text */`

All whitespace and comments are ignored during parsing.

### Lexer Implementation

The lexer is built using Megaparsec's built-in lexer utilities:

```haskell
-- Space consumer (handles whitespace and comments)
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

-- Lexeme parser (parses token and trailing whitespace)
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Symbol parser (parses specific string)
symbol :: String -> Parser String
symbol = L.symbol sc

-- Identifier parser
identifier :: Parser String
identifier = lexeme (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` keywords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

-- Integer parser
integer :: Parser Int64
integer = lexeme L.decimal

-- Keyword parser
keyword :: String -> Parser ()
keyword w = lexeme (string w *> notFollowedBy alphaNumChar)
```

## Syntactic Analysis

### Parser Combinators

Parser combinators allow building complex parsers from simple ones:

```haskell
-- Sequence: parse p, then q
p <*> q

-- Alternative: try p, if it fails try q
p <|> q

-- Map: parse p and apply function f
f <$> p

-- Bind: parse p, use result in computation
p >>= \result -> ...

-- Many: parse p zero or more times
many p

-- Some: parse p one or more times
some p

-- Optional: parse p or return Nothing
optional p
```

### Expression Parsing

GLaDOS uses **operator precedence parsing** for expressions:

```haskell
-- Expression parser with precedence
expr :: Parser Expr
expr = makeExprParser term operatorTable

-- Operator precedence table
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ binary "*" Mul, binary "/" Div ]                    -- Precedence 6 (highest)
  , [ binary "+" Add, binary "-" Sub ]                    -- Precedence 5
  , [ binary "<" Lt, binary ">" Gt
    , binary "<=" Lte, binary ">=" Gte ]                  -- Precedence 4
  , [ binary "==" Eq, binary "!=" Neq ]                   -- Precedence 3
  , [ binary "&&" And ]                                    -- Precedence 2
  , [ binary "||" Or ]                                     -- Precedence 1 (lowest)
  ]

-- Binary operator helper
binary :: String -> BinOp -> Operator Parser Expr
binary name op = InfixL (BinOp op <$ symbol name)

-- Term (atomic expression)
term :: Parser Expr
term = parens expr
   <|> Lit <$> literal
   <|> try functionCall
   <|> Var <$> identifier
```

**Key Points:**
- Operators at the top of the table bind tighter (higher precedence)
- All operators are left-associative (`InfixL`)
- Terms include literals, variables, function calls, and parenthesized expressions

### Statement Parsing

Statements are parsed using pattern matching:

```haskell
statement :: Parser Statement
statement = choice
  [ ifStatement
  , whileStatement
  , returnStatement
  , try varDeclaration
  , try assignment
  , expressionStatement
  ]

-- If statement
ifStatement :: Parser Statement
ifStatement = do
  keyword "if"
  condition <- parens expr
  thenBody <- braces (many statement)
  elseBody <- optional (keyword "else" >> braces (many statement))
  return $ If condition thenBody elseBody

-- While loop
whileStatement :: Parser Statement
whileStatement = do
  keyword "while"
  condition <- parens expr
  body <- braces (many statement)
  return $ While condition body

-- Return statement
returnStatement :: Parser Statement
returnStatement = do
  keyword "return"
  value <- expr
  optional (symbol ";")
  return $ Return value

-- Variable declaration
varDeclaration :: Parser Statement
varDeclaration = do
  varType <- typeParser
  varName <- identifier
  value <- optional (symbol "=" >> expr)
  optional (symbol ";")
  return $ VarDecl varType varName value

-- Assignment
assignment :: Parser Statement
assignment = do
  varName <- identifier
  symbol "="
  value <- expr
  optional (symbol ";")
  return $ Assign varName value

-- Expression statement (function call as statement)
expressionStatement :: Parser Statement
expressionStatement = do
  e <- expr
  optional (symbol ";")
  return $ ExprStmt e
```

### Function Parsing

Functions are the top-level program structure:

```haskell
-- Function definition
functionDef :: Parser FuncDef
functionDef = do
  returnType <- typeParser
  name <- identifier
  params <- parens paramList
  body <- braces (many statement)
  return $ FuncDef
    { funcName = name
    , funcReturnType = returnType
    , funcParams = params
    , funcBody = body
    }

-- Parameter list
paramList :: Parser [(String, Type)]
paramList = param `sepBy` symbol ","
  where
    param = (,) <$> typeParser <*> identifier

-- Type parser
typeParser :: Parser Type
typeParser = (TInt <$ keyword "int") <|> (TBool <$ keyword "bool")

-- Program (list of functions)
program :: Parser Program
program = sc *> many functionDef <* eof
```

### Function Calls

Function calls require special handling:

```haskell
functionCall :: Parser Expr
functionCall = do
  name <- identifier
  args <- parens (expr `sepBy` symbol ",")
  return $ Call name args
```

**Note:** `try` combinator is used to backtrack if parsing fails (useful for distinguishing variable from function call).

## AST Structure

The parser produces an Abstract Syntax Tree with the following structure:

### Program Node

```haskell
data Program = Program [FuncDef]
```

### Function Definition Node

```haskell
data FuncDef = FuncDef
  { funcName :: String
  , funcReturnType :: Type
  , funcParams :: [(String, Type)]
  , funcBody :: [Statement]
  }
```

### Statement Nodes

```haskell
data Statement
  = VarDecl Type String (Maybe Expr)  -- int x = 5;
  | Assign String Expr                 -- x = 10;
  | If Expr [Statement] (Maybe [Statement])  -- if (...) {...} else {...}
  | While Expr [Statement]             -- while (...) {...}
  | Return Expr                        -- return x;
  | ExprStmt Expr                      -- functionCall();
```

### Expression Nodes

```haskell
data Expr
  = Lit Literal                        -- 42, true
  | Var String                         -- x
  | Call String [Expr]                 -- foo(a, b)
  | BinOp BinOp Expr Expr              -- a + b
```

### Binary Operators

```haskell
data BinOp
  = Add | Sub | Mul | Div              -- Arithmetic
  | Eq | Neq | Lt | Gt | Lte | Gte     -- Comparison
  | And | Or                           -- Logical
```

### Literals

```haskell
data Literal
  = IntLit Int64                       -- 42
  | BoolLit Bool                       -- true, false
```

### Types

```haskell
data Type
  = TInt                               -- int
  | TBool                              -- bool
```

## Error Handling

### Parse Errors

Megaparsec provides detailed error messages:

```haskell
-- Example error messages
Parse error at line 5, column 10:
  unexpected '{'
  expecting ';', ')', or identifier

Parse error at line 10:
  unexpected end of input
  expecting statement
```

### Error Recovery

The parser uses:
- **`try` combinator**: Backtrack on failure
- **`<|>` operator**: Try alternative parsers
- **`optional`**: Make parts of syntax optional (e.g., semicolons)

### Source Position Tracking

Megaparsec automatically tracks:
- Line number
- Column number
- File name (if provided)

This information is included in error messages.

## Parser Usage

### Parsing Source Code

```haskell
-- Parse from string
parseGLaDOS :: String -> Either String Program
parseGLaDOS input =
  case parse program "<stdin>" input of
    Left err -> Left (errorBundlePretty err)
    Right ast -> Right ast
```

### Example

**Input:**
```c
int add(int a, int b) {
    return a + b;
}
```

**Output AST:**
```haskell
Program
  [ FuncDef
      { funcName = "add"
      , funcReturnType = TInt
      , funcParams = [("a", TInt), ("b", TInt)]
      , funcBody =
          [ Return
              (BinOp Add
                (Var "a")
                (Var "b")
              )
          ]
      }
  ]
```

## Parser Features

### Semicolon Flexibility

GLaDOS makes semicolons **optional** in most contexts:

```haskell
optional (symbol ";")
```

This allows both styles:

```c
// With semicolons
int x = 5;
return x;

// Without semicolons
int x = 5
return x
```

### Expression Precedence

The operator table ensures correct precedence:

```c
// Parsed as: 2 + (3 * 4)
int x = 2 + 3 * 4;

// Parsed as: (5 > 3) && (10 < 20)
bool b = 5 > 3 && 10 < 20;
```

### Nested Expressions

The parser handles arbitrary nesting:

```c
int result = ((a + b) * c) / (d - e);
bool flag = (x > 0) && ((y < 10) || (z == 5));
```

### Recursive Functions

Functions can call themselves:

```c
int factorial(int n) {
    if (n <= 1) {
        return 1;
    }
    return n * factorial(n - 1);
}
```

## Parser Limitations

### No Forward Declarations

Functions must be defined before being called. The type checker resolves this by building a global environment first.

### No Mutual Recursion

Direct mutual recursion is not supported (but can be achieved through careful ordering).

### Limited Error Recovery

The parser stops at the first error. It doesn't attempt to recover and continue parsing.

## Implementation Details

### Megaparsec Advantages

1. **Composability**: Build complex parsers from simple ones
2. **Error Messages**: Detailed, helpful error reporting
3. **Performance**: Efficient parsing with minimal overhead
4. **Type Safety**: Strong typing prevents many parser bugs
5. **Flexibility**: Easy to extend with new syntax

### Parser Performance

- **Time Complexity**: O(n) where n is input length
- **Space Complexity**: O(n) for AST storage
- **Backtracking**: Minimal, limited to specific cases (function calls vs variables)

## Testing the Parser

### Example Test Cases

```haskell
-- Test: Simple function
testSimpleFunction = "int main() { return 42; }"

-- Test: Complex expression
testExpression = "int x = 2 + 3 * 4 - 5 / 2;"

-- Test: Control flow
testControlFlow = "if (x > 0) { return 1; } else { return 0; }"

-- Test: Recursion
testRecursion = "int fib(int n) { if (n <= 1) { return n; } return fib(n-1) + fib(n-2); }"
```

### Running Tests

```bash
stack test --test-arguments="--match Parser"
```

## Next Steps

After parsing, the AST is passed to:

1. **Security Analysis**: Check for division by zero and other issues
2. **Type Checking**: Verify type correctness
3. **IR Generation**: Convert to bytecode

See:
- [Type Checking](./type-checking.md)
- [IR Generation](./ir-generation.md)
