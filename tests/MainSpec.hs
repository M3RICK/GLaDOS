module MainSpec (spec) where

import Test.Hspec
import System.Process
import System.Exit
import Control.Exception (catch, IOException)

-- Helper function to run glados with a file argument
runGladosFile :: String -> IO (Either String String)
runGladosFile filename = do
  result <- catch 
    (readProcessWithExitCode "./glados" ["tests/scm/" ++ filename] "")
    (\e -> return (ExitFailure 84, "", show (e :: IOException)))
  case result of
    (ExitSuccess, notStdout, _) -> return $ Right (trim notStdout)
    (ExitFailure _, _, notStderr) -> return $ Left notStderr

-- Helper function to run glados with stdin input
runGladosStdin :: String -> IO (Either String String)
runGladosStdin input = do
  result <- catch 
    (readProcessWithExitCode "./glados" [] input)
    (\e -> return (ExitFailure 84, "", show (e :: IOException)))
  case result of
    (ExitSuccess, notStdout, _) -> return $ Right (trim notStdout)
    (ExitFailure _, _, notStderr) -> return $ Left notStderr
    
-- Helper to trim whitespace
trim :: String -> String
trim = reverse . dropWhile (`elem` " \n\r\t") . reverse . dropWhile (`elem` " \n\r\t")

spec :: Spec
spec = do
  describe "GLaDOS LISP Interpreter - Part 1 Features" $ do
    
    describe "64-bit Integer Types" $ do
      it "should handle large positive integers via file" $ do
        result <- runGladosFile "large_integers.scm"
        result `shouldBe` Right "9223372036854775806"  -- Max 64-bit - 1
      
      it "should handle large negative integers via stdin" $ do
        result <- runGladosStdin "-9223372036854775807"
        result `shouldBe` Right "-9223372036854775807"

    describe "Boolean Values (#t/#f)" $ do
      it "should handle boolean literals via file" $ do
        result <- runGladosFile "booleans.scm" 
        result `shouldBe` Right "#t"
        
      it "should handle boolean comparison via stdin" $ do
        result <- runGladosStdin "(eq? #t #t)\n(eq? #f #f)"
        result `shouldBe` Right "#t"

    describe "Variable Bindings (define)" $ do
      it "should handle simple variable definition via file" $ do
        result <- runGladosFile "simple_define.scm"
        result `shouldBe` Right "100"
        
      it "should handle multiple variable definitions via stdin" $ do
        result <- runGladosStdin "(define x 10)\n(define y 20)\n(+ x y)"
        result `shouldBe` Right "30"

    describe "Lambda Functions" $ do
      it "should handle lambda definition and call via file" $ do
        result <- runGladosFile "lambda_test.scm"
        result `shouldBe` Right "15"
        
      it "should handle anonymous lambda call via stdin" $ do
        result <- runGladosStdin "((lambda (x y) (* x y)) 6 7)"
        result `shouldBe` Right "42"

    describe "Named Function Definitions" $ do
      it "should handle named function definition via file" $ do
        result <- runGladosFile "named_function.scm"
        result `shouldBe` Right "25"
        
      it "should handle function with multiple parameters via stdin" $ do
        result <- runGladosStdin "(define (multiply a b c) (* a (* b c)))\n(multiply 2 3 4)"
        result `shouldBe` Right "24"

    describe "Recursion Support" $ do
      it "should handle recursive factorial via file" $ do
        result <- runGladosFile "factorial.scm"
        result `shouldBe` Right "120"  -- factorial(5)
        
      it "should handle recursive fibonacci via stdin" $ do
        result <- runGladosStdin "(define (fib n)\n  (if (< n 2)\n      n\n      (+ (fib (- n 1)) (fib (- n 2)))))\n(fib 7)"
        result `shouldBe` Right "13"  -- 7th fibonacci number

    describe "Conditional Expressions (if)" $ do
      it "should handle nested conditionals via file" $ do
        result <- runGladosFile "nested_if.scm"
        result `shouldBe` Right "positive"
        
      it "should handle conditional with computation via stdin" $ do
        result <- runGladosStdin "(if (< 10 20)\n    (+ 5 5)\n    (- 5 5))"
        result `shouldBe` Right "10"

    describe "Arithmetic Built-ins" $ do
      it "should handle all arithmetic operations via file" $ do
        result <- runGladosFile "all_arithmetic.scm"
        result `shouldBe` Right "3"  -- final mod operation result
        
      it "should handle complex arithmetic expression via stdin" $ do
        result <- runGladosStdin "(+ (* 3 4) (div 20 5) (mod 17 6))"
        result `shouldBe` Right "21"  -- 12 + 4 + 5

    describe "Predicate Built-ins" $ do
      it "should handle equality and comparison via file" $ do
        result <- runGladosFile "predicates.scm"
        result `shouldBe` Right "#t"
        
      it "should handle complex comparisons via stdin" $ do
        result <- runGladosStdin "(< (+ 2 3) (* 2 4))"
        result `shouldBe` Right "#t"  -- 5 < 8

    describe "Function Calls and Application" $ do
      it "should handle nested function calls via file" $ do
        result <- runGladosFile "nested_calls.scm"
        result `shouldBe` Right "16"
        
      it "should handle function composition via stdin" $ do
        result <- runGladosStdin "(define (square x) (* x x))\n(define (double x) (+ x x))\n(square (double 3))"
        result `shouldBe` Right "36"  -- square(double(3)) = square(6) = 36

    describe "Error Handling and Exit Codes" $ do
      it "should handle unbound variables with exit 84" $ do
        (exitCode, _, _) <- readProcessWithExitCode "./glados" [] "undefined_variable"
        exitCode `shouldBe` ExitFailure 84
        
      it "should handle malformed syntax with exit 84" $ do
        (exitCode, _, _) <- readProcessWithExitCode "./glados" [] "(+ 1 2"
        exitCode `shouldBe` ExitFailure 84
        
      it "should handle division by zero with exit 84" $ do
        (exitCode, _, _) <- readProcessWithExitCode "./glados" [] "(div 5 0)"
        exitCode `shouldBe` ExitFailure 84

    describe "S-Expression Parsing" $ do
      it "should handle deeply nested expressions via file" $ do
        result <- runGladosFile "nested_sexpr.scm"
        result `shouldBe` Right "42"
        
      it "should handle whitespace variations via stdin" $ do
        result <- runGladosStdin "(+    1\n\n    2    3\t\t4)"
        result `shouldBe` Right "10"