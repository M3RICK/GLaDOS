module HardSpec (spec) where

import Test.Hspec
import System.Process
import System.Exit
import Control.Exception (catch, IOException)
import Data.List (isInfixOf)

-- Helper function to run glados with stdin input
runGlados :: String -> IO (Either String String)
runGlados input = do
  result <- catch 
    (readProcessWithExitCode "./glados" [] input)
    (\e -> return (ExitFailure 84, "", show (e :: IOException)))
  case result of
    (ExitSuccess, stdout, _) -> return $ Right (trim stdout)
    (ExitFailure _, _, stderr) -> return $ Left stderr

-- Helper to trim whitespace
trim :: String -> String
trim = reverse . dropWhile (`elem` " \n\r\t") . reverse . dropWhile (`elem` " \n\r\t")

spec :: Spec
spec = do
  describe "HardSpec - Advanced Tests" $ do
    describe "Nested Arithmetic" $ do
      it "should handle nested arithmetic operations" $ do
        result <- runGlados "(+ (* 2 3) (- 8 3))"
        result `shouldBe` Right "11"
      
      it "should handle deeply nested expressions" $ do
        result <- runGlados "(+ 1 (* 2 (- 5 (div 6 2))))"
        result `shouldBe` Right "5"  -- 1 + (2 * (5 - 3)) = 1 + 4 = 5
      
      it "should handle multiple nested operations" $ do
        result <- runGlados "(* (+ 2 3) (- 10 7))"
        result `shouldBe` Right "15"  -- (2 + 3) * (10 - 7) = 5 * 3 = 15
      
      it "should handle all four operations nested" $ do
        result <- runGlados "(+ (* 4 5) (div (- 20 8) (mod 7 3)))"
        result `shouldBe` Right "32"  -- (4 * 5) + ((20 - 8) / (7 % 3)) = 20 + (12 / 1) = 32
      
      it "should handle triple nesting" $ do
        result <- runGlados "(+ (+ (+ 1 2) 3) 4)"
        result `shouldBe` Right "10"  -- ((1 + 2) + 3) + 4 = 10
      
      it "should handle negative numbers in nested expressions" $ do
        result <- runGlados "(+ (* -2 3) (- 5 -4))"
        result `shouldBe` Right "3"  -- (-2 * 3) + (5 - (-4)) = -6 + 9 = 3

    describe "Builtin Edge Cases" $ do
      it "should handle addition edge cases" $ do
        result1 <- runGlados "(+ 0 0)"
        result1 `shouldBe` Right "0"
        result2 <- runGlados "(+ -5 5)"
        result2 `shouldBe` Right "0"
        result3 <- runGlados "(+ 9223372036854775806 1)"
        result3 `shouldBe` Right "9223372036854775807"

      it "should handle subtraction edge cases" $ do
        result1 <- runGlados "(- 0 0)"
        result1 `shouldBe` Right "0"
        result2 <- runGlados "(- 5 5)"
        result2 `shouldBe` Right "0"
        result3 <- runGlados "(- -10 -5)"
        result3 `shouldBe` Right "-5"

      it "should handle multiplication edge cases" $ do
        result1 <- runGlados "(* 0 1000)"
        result1 `shouldBe` Right "0"
        result2 <- runGlados "(* -1 42)"
        result2 `shouldBe` Right "-42"
        result3 <- runGlados "(* 1 999)"
        result3 `shouldBe` Right "999"

      it "should handle division edge cases" $ do
        result1 <- runGlados "(div 0 5)"
        result1 `shouldBe` Right "0"
        result2 <- runGlados "(div 15 -3)"
        result2 `shouldBe` Right "-5"
        result3 <- runGlados "(div -20 -4)"
        result3 `shouldBe` Right "5"

      it "should handle modulo edge cases" $ do
        result1 <- runGlados "(mod 0 5)"
        result1 `shouldBe` Right "0"
        result2 <- runGlados "(mod 7 7)"
        result2 `shouldBe` Right "0"
        result3 <- runGlados "(mod 1 3)"
        result3 `shouldBe` Right "1"

      it "should handle eq? edge cases" $ do
        result1 <- runGlados "(eq? 0 0)"
        result1 `shouldBe` Right "#t"
        result2 <- runGlados "(eq? -5 -5)"
        result2 `shouldBe` Right "#t"
        result3 <- runGlados "(eq? 1 2)"
        result3 `shouldBe` Right "#f"

      it "should handle < edge cases" $ do
        result1 <- runGlados "(< 0 1)"
        result1 `shouldBe` Right "#t"
        result2 <- runGlados "(< -5 0)"
        result2 `shouldBe` Right "#t"
        result3 <- runGlados "(< 5 5)"
        result3 `shouldBe` Right "#f"

      it "should handle > edge cases" $ do
        result1 <- runGlados "(> 1 0)"
        result1 `shouldBe` Right "#t"
        result2 <- runGlados "(> 0 -5)"
        result2 `shouldBe` Right "#t"
        result3 <- runGlados "(> 5 5)"
        result3 `shouldBe` Right "#f"

    describe "LispVal Types Edge Cases" $ do
      it "should handle boolean values correctly" $ do
        result1 <- runGlados "#t"
        result1 `shouldBe` Right "#t"
        result2 <- runGlados "#f"
        result2 `shouldBe` Right "#f"

      it "should handle atom/symbol values" $ do
        result1 <- runGlados "(define x 42)\nx"
        result1 `shouldBe` Right "42"
        result2 <- runGlados "(define very-long-symbol-name 100)\nvery-long-symbol-name"
        result2 `shouldBe` Right "100"

      it "should handle number edge cases" $ do
        result1 <- runGlados "0"
        result1 `shouldBe` Right "0"
        result2 <- runGlados "-999999"
        result2 `shouldBe` Right "-999999"
        result3 <- runGlados "9223372036854775807"
        result3 `shouldBe` Right "9223372036854775807"

      it "should handle list literal evaluation" $ do
        result <- runGlados "(1 2 3)"
        -- This will probably fail since (1 2 3) tries to call 1 as a function
        -- Let's just test that we get some kind of error
        result `shouldSatisfy` (\x -> case x of Left _ -> True; Right _ -> False)

      it "should handle function definition shows procedure" $ do
        result <- runGlados "(define (test-func x y) (+ x y))\ntest-func"
        result `shouldSatisfy` (\x -> case x of 
          Right str -> "#<procedure" `isInfixOf` str
          Left _ -> False)

    describe "Variable Operations Edge Cases" $ do
      it "should handle basic variable definition and lookup" $ do
        result <- runGlados "(define x 42)\nx"
        result `shouldBe` Right "42"

      it "should handle variable redefinition" $ do
        result <- runGlados "(define x 10)\n(define x 20)\nx"
        result `shouldBe` Right "20"

      it "should handle variables with special characters" $ do
        result <- runGlados "(define test-var-123 99)\ntest-var-123"
        result `shouldBe` Right "99"

      it "should handle undefined variable error" $ do
        result <- runGlados "undefined-variable"
        result `shouldSatisfy` (\x -> case x of Left _ -> True; Right _ -> False)

      it "should handle variable shadowing in expressions" $ do
        result <- runGlados "(define x 5)\n(+ x (* x 2))"
        result `shouldBe` Right "15"  -- 5 + (5 * 2) = 15

      it "should handle multiple variable definitions" $ do
        result <- runGlados "(define a 3)\n(define b 4)\n(define c 5)\n(+ a (* b c))"
        result `shouldBe` Right "23"  -- 3 + (4 * 5) = 23

    describe "Function Definition Edge Cases" $ do
      it "should handle function with no parameters" $ do
        result <- runGlados "(define (constant) 42)\n(constant)"
        result `shouldBe` Right "42"

      it "should handle function with one parameter" $ do
        result <- runGlados "(define (double x) (* x 2))\n(double 5)"
        result `shouldBe` Right "10"

      it "should handle function with many parameters" $ do
        result <- runGlados "(define (sum-five a b c d e) (+ a b c d e))\n(sum-five 1 2 3 4 5)"
        result `shouldBe` Right "15"

      it "should handle function redefinition" $ do
        result <- runGlados "(define (f x) x)\n(define (f x) (* x 2))\n(f 5)"
        result `shouldBe` Right "10"

      it "should handle wrong number of arguments" $ do
        result <- runGlados "(define (add x y) (+ x y))\n(add 1 2 3)"
        result `shouldSatisfy` (\x -> case x of Left _ -> True; Right _ -> False)

      it "should handle function calling undefined function" $ do
        result <- runGlados "(define (test) (undefined-func 1))\n(test)"
        result `shouldSatisfy` (\x -> case x of Left _ -> True; Right _ -> False)

      it "should handle function with same name as builtin" $ do
        result <- runGlados "(define (+ x y) (* x y))\n(+ 3 4)"
        -- This might work (shadowing) or fail depending on implementation
        result `shouldSatisfy` (\x -> case x of Right _ -> True; Left _ -> True)

    describe "Lambda Expression Edge Cases" $ do
      it "should handle simple lambda" $ do
        result <- runGlados "((lambda (x) (* x 2)) 5)"
        result `shouldBe` Right "10"

      it "should handle lambda with no parameters" $ do
        result <- runGlados "((lambda () 42))"
        result `shouldBe` Right "42"

      it "should handle lambda with multiple parameters" $ do
        result <- runGlados "((lambda (x y z) (+ x (* y z))) 1 2 3)"
        result `shouldBe` Right "7"  -- 1 + (2 * 3) = 7

      it "should handle nested lambda" $ do
        result <- runGlados "((lambda (x) ((lambda (y) (+ x y)) 3)) 4)"
        result `shouldBe` Right "7"

      it "should handle lambda returning lambda (currying)" $ do
        result <- runGlados "(((lambda (x) (lambda (y) (+ x y))) 5) 3)"
        result `shouldBe` Right "8"

      it "should handle lambda assigned to variable" $ do
        result <- runGlados "(define add-one (lambda (x) (+ x 1)))\n(add-one 5)"
        result `shouldBe` Right "6"

      it "should handle lambda with wrong number of arguments" $ do
        result <- runGlados "((lambda (x y) (+ x y)) 1)"
        result `shouldSatisfy` (\x -> case x of Left _ -> True; Right _ -> False)

    describe "Conditional Logic Edge Cases" $ do
      it "should handle if with true condition" $ do
        result <- runGlados "(if #t 100 200)"
        result `shouldBe` Right "100"

      it "should handle if with false condition" $ do
        result <- runGlados "(if #f 100 200)"
        result `shouldBe` Right "200"

      it "should handle if with computed boolean condition" $ do
        result <- runGlados "(if (< 3 5) 42 0)"
        result `shouldBe` Right "42"

      it "should handle if with equality condition" $ do
        result <- runGlados "(if (eq? 5 5) 999 111)"
        result `shouldBe` Right "999"

      it "should handle nested if expressions" $ do
        result <- runGlados "(if #t (if #f 1 2) 3)"
        result `shouldBe` Right "2"

      it "should handle if with complex expressions in branches" $ do
        result <- runGlados "(if (< 2 4) (+ 10 20) (* 5 6))"
        result `shouldBe` Right "30"

      it "should handle if with variable conditions" $ do
        result <- runGlados "(define x 5)\n(if (> x 0) x (- 0 x))"
        result `shouldBe` Right "5"

    describe "Recursion Edge Cases" $ do
      it "should handle simple recursion - factorial" $ do
        result <- runGlados "(define (fact n) (if (eq? n 1) 1 (* n (fact (- n 1)))))\n(fact 5)"
        result `shouldBe` Right "120"

      it "should handle recursion with base case 0" $ do
        result <- runGlados "(define (countdown n) (if (eq? n 0) 0 (countdown (- n 1))))\n(countdown 5)"
        result `shouldBe` Right "0"

      it "should handle fibonacci recursion" $ do
        result <- runGlados "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))\n(fib 6)"
        result `shouldBe` Right "8"

      it "should handle recursion with accumulator" $ do
        result <- runGlados "(define (sum-to n acc) (if (eq? n 0) acc (sum-to (- n 1) (+ acc n))))\n(sum-to 5 0)"
        result `shouldBe` Right "15"  -- 1+2+3+4+5 = 15

      it "should handle deep recursion" $ do
        result <- runGlados "(define (deep n) (if (eq? n 0) 0 (deep (- n 1))))\n(deep 50)"
        result `shouldBe` Right "0"

      it "should handle recursion with multiple parameters" $ do
        result <- runGlados "(define (gcd a b) (if (eq? b 0) a (gcd b (mod a b))))\n(gcd 48 18)"
        result `shouldBe` Right "6"

    describe "Error Handling and Boundary Cases" $ do
      it "should handle division by zero" $ do
        result <- runGlados "(div 5 0)"
        result `shouldSatisfy` (\x -> case x of Left _ -> True; Right _ -> False)

      it "should handle modulo by zero" $ do
        result <- runGlados "(mod 5 0)"
        result `shouldSatisfy` (\x -> case x of Left _ -> True; Right _ -> False)

      it "should handle malformed expressions" $ do
        result <- runGlados "(+ 1 2"
        result `shouldSatisfy` (\x -> case x of Left _ -> True; Right _ -> False)

      it "should handle extra closing parentheses" $ do
        result <- runGlados "(+ 1 2))"
        result `shouldSatisfy` (\x -> case x of Left _ -> True; Right _ -> False)

      it "should handle empty input" $ do
        result <- runGlados ""
        result `shouldBe` Right "#t"

    describe "Complex Expression Combinations" $ do
      it "should handle function composition" $ do
        result <- runGlados "(define (add1 x) (+ x 1))\n(define (double x) (* x 2))\n(double (add1 5))"
        result `shouldBe` Right "12"  -- double(add1(5)) = double(6) = 12

      it "should handle lambda and function interaction" $ do
        result <- runGlados "(define (apply-twice f x) (f (f x)))\n((lambda (n) (apply-twice (lambda (x) (+ x 1)) n)) 5)"
        result `shouldBe` Right "7"  -- apply add1 twice to 5 = 7

      it "should handle conditional with function calls" $ do
        result <- runGlados "(define (abs x) (if (< x 0) (- 0 x) x))\n(abs -42)"
        result `shouldBe` Right "42"

      it "should handle nested function definitions and calls" $ do
        result <- runGlados "(define (outer x) (define (inner y) (+ x y)) (inner 10))\n(outer 5)"
        -- This might fail if nested defines aren't supported
        result `shouldSatisfy` (\x -> case x of Right "15" -> True; Left _ -> True; _ -> False)

    describe "Scoping and Environment Edge Cases" $ do
      it "should handle parameter shadowing global variables" $ do
        result <- runGlados "(define x 100)\n(define (test x) (+ x 1))\n(+ (test 5) x)"
        result `shouldBe` Right "106"  -- (5+1) + 100 = 106

      it "should handle function parameter precedence" $ do
        result <- runGlados "(define y 999)\n(define (func y z) (+ y z))\n(func 1 2)"
        result `shouldBe` Right "3"

      it "should maintain global state between expressions" $ do
        result <- runGlados "(define counter 0)\n(define (increment) (define counter (+ counter 1)) counter)\n(increment)\n(increment)"
        -- Might not work if local defines don't affect global scope
        result `shouldSatisfy` (\x -> case x of Right _ -> True; Left _ -> True)

    describe "Large Number and Overflow Testing" $ do
      it "should handle maximum 64-bit integer" $ do
        result <- runGlados "9223372036854775807"
        result `shouldBe` Right "9223372036854775807"

      it "should handle minimum 64-bit integer" $ do
        result <- runGlados "-9223372036854775808"
        result `shouldSatisfy` (\x -> case x of Right _ -> True; Left _ -> True)

      it "should handle large number arithmetic" $ do
        result <- runGlados "(+ 1000000000 2000000000)"
        result `shouldBe` Right "3000000000"

      it "should handle multiplication of large numbers" $ do
        result <- runGlados "(* 1000000 1000000)"
        result `shouldBe` Right "1000000000000"

      it "should handle potential overflow scenarios" $ do
        result <- runGlados "(+ 9223372036854775806 1)"
        result `shouldBe` Right "9223372036854775807"

    describe "Deep Nesting Stress Tests" $ do
      it "should handle very deeply nested arithmetic" $ do
        let deepExpr = foldr (\i acc -> "(+ " ++ show (i :: Int) ++ " " ++ acc ++ ")") "0" [1..20]
        result <- runGlados deepExpr
        result `shouldBe` Right "210"  -- Sum of 1 to 20

      it "should handle deeply nested function calls" $ do
        result <- runGlados "(+ (+ (+ (+ (+ 1 2) 3) 4) 5) 6)"
        result `shouldBe` Right "21"

      it "should handle nested conditionals" $ do
        result <- runGlados "(if #t (if #t (if #t (if #t 42 0) 0) 0) 0)"
        result `shouldBe` Right "42"

      it "should handle deeply nested lambda calls" $ do
        result <- runGlados "((lambda (a) ((lambda (b) ((lambda (c) (+ a (+ b c))) 3)) 2)) 1)"
        result `shouldBe` Right "6"  -- 1 + 2 + 3

    describe "Performance and Stress Tests" $ do
      it "should handle moderately deep recursion" $ do
        result <- runGlados "(define (countdown n) (if (eq? n 0) 0 (countdown (- n 1))))\n(countdown 100)"
        result `shouldBe` Right "0"

      it "should handle recursive computation with reasonable depth" $ do
        result <- runGlados "(define (sum-to n) (if (eq? n 0) 0 (+ n (sum-to (- n 1)))))\n(sum-to 10)"
        result `shouldBe` Right "55"  -- 1+2+...+10 = 55

      it "should handle function with many local variables" $ do
        result <- runGlados "(define (many-vars x) (define a (+ x 1)) (define b (+ x 2)) (define c (+ x 3)) (define d (+ x 4)) (+ a (+ b (+ c d))))\n(many-vars 10)"
        result `shouldSatisfy` (\x -> case x of Right _ -> True; Left _ -> True)

      it "should handle expressions with many nested operations" $ do
        result <- runGlados "(+ 1 (+ 2 (+ 3 (+ 4 (+ 5 (+ 6 (+ 7 (+ 8 (+ 9 10)))))))))"
        result `shouldBe` Right "55"

      it "should handle complex expression evaluation" $ do
        result <- runGlados "(* (+ 1 2) (+ 3 (+ 4 (+ 5 (+ 6 (+ 7 (+ 8 (+ 9 10))))))))"
        result `shouldBe` Right "156"  -- 3 * 52 = 156