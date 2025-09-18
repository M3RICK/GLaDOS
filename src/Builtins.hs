module Builtins (initialEnv) where
import qualified Data.Map as Map
import Types

initialEnv :: Env
initialEnv = Map.fromList
  [ ("+", Builtin addFunc)
  , ("-", Builtin subFunc)
  , ("*", Builtin mulFunc)
  , ("div", Builtin divFunc)
  , ("mod", Builtin modFunc)
  , ("eq?", Builtin eqFunc)
  , ("<", Builtin ltFunc)
  , (">", Builtin gtFunc)
  ]

addFunc :: [LispVal] -> Either String LispVal
addFunc [Number x, Number y] = Right (Number (x + y))
addFunc [Number _, _] = Left "Type error: expected Number"
addFunc [Number _] = Left "Addition requires 2 arguments, got 1"
addFunc _ = Left "Addition requires exactly 2 arguments"

subFunc :: [LispVal] -> Either String LispVal
subFunc [Number x, Number y] = Right (Number (x - y))
subFunc [Number _, _] = Left "Type error: expected Number"
subFunc [Number _] = Left "Subtraction requires 2 arguments, got 1"
subFunc _ = Left "Subtraction requires exactly 2 arguments"

mulFunc :: [LispVal] -> Either String LispVal
mulFunc [Number x, Number y] = Right (Number (x * y))
mulFunc [Number _, _] = Left "Type error: expected Number"
mulFunc [Number _] = Left "Multiplication requires 2 arguments, got 1"
mulFunc _ = Left "Multiplication requires exactly 2 arguments"

divFunc :: [LispVal] -> Either String LispVal
divFunc [Number _, Number 0] = Left "Division by 0 is illegal"
divFunc [Number x, Number y] = Right (Number (x `div` y))
divFunc [Number _, _] = Left "Type error: expected Number"
divFunc [Number _] = Left "Division requires 2 arguments, got 1"
divFunc _ = Left "Division requires exactly 2 arguments"

modFunc :: [LispVal] -> Either String LispVal
modFunc [Number _, Number 0] = Left "Modulo by 0 is illegal"
modFunc [Number x, Number y] = Right (Number (x `mod` y))
modFunc [Number _, _] = Left "Type error: expected Number"
modFunc [Number _] = Left "Modulo requires 2 arguments, got 1"
modFunc _ = Left "Modulo requires exactly 2 arguments"

eqFunc :: [LispVal] -> Either String LispVal
eqFunc [Number x, Number y] = Right (Bool (x == y))
eqFunc [Bool x, Bool y] = Right (Bool (x == y))
eqFunc [Atom x, Atom y] = Right (Bool (x == y))
eqFunc [x, y] = Right (Bool (x == y))
eqFunc [_] = Left "Equivalency requires 2 arguments, got 1"
eqFunc _ = Left "Equivalency requires exactly 2 arguments"

ltFunc :: [LispVal] -> Either String LispVal
ltFunc [Number x, Number y] = Right (Bool (x < y))
ltFunc [Number _, _] = Left "Type error: expected Number"
ltFunc [Number _] = Left "Less-than requires 2 arguments, got 1"
ltFunc _ = Left "Less-than requires exactly 2 arguments"

gtFunc :: [LispVal] -> Either String LispVal
gtFunc [Number x, Number y] = Right (Bool (x > y))
gtFunc [Number _, _] = Left "Type error: expected Number"
gtFunc [Number _] = Left "greater-than requires 2 arguments, got 1"
gtFunc _ = Left "greater-than requires exactly 2 arguments"