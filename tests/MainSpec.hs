module MainSpec (spec) where

import Test.Hspec
import System.Process
import System.Exit
import Control.Exception (catch, IOException)

runGladosFile :: String -> IO (Either String String)
runGladosFile filename = do
  result <- catch 
    (readProcessWithExitCode "./glados" ["scm/" ++ filename] "")
    (\e -> return (ExitFailure 84, "", show (e :: IOException)))
  case result of
    (ExitSuccess, notStdout, _) -> return $ Right (trim notStdout)
    (ExitFailure _, _, notStderr) -> return $ Left notStderr

runGladosStdin :: String -> IO (Either String String)
runGladosStdin input = do
  result <- catch 
    (readProcessWithExitCode "./glados" [] input)
    (\e -> return (ExitFailure 84, "", show (e :: IOException)))
  case result of
    (ExitSuccess, notStdout, _) -> return $ Right (trim notStdout)
    (ExitFailure _, _, notStderr) -> return $ Left notStderr
    
trim :: String -> String
trim = reverse . dropWhile (`elem` " \n\r\t") . reverse . dropWhile (`elem` " \n\r\t")

spec :: Spec
spec = do
  describe "GLaDOS Main Executable" $ do
    
    describe "File Input Tests" $ do
      it "should handle arithmetic via file" $ do
        result <- runGladosFile "Arithmetic.scm"
        result `shouldBe` Right "2"

      it "should handle booleans via file" $ do
        result <- runGladosFile "Boolean.scm"
        result `shouldBe` Right "#f"

      it "should handle conditionals via file" $ do
        result <- runGladosFile "Conditionals.scm"
        result `shouldBe` Right "42"

      it "should handle variables via file" $ do
        result <- runGladosFile "Multiple.scm"
        result `shouldBe` Right "30"

      it "should handle nested expressions via file" $ do
        result <- runGladosFile "Nested.scm"
        result `shouldBe` Right "28"

      it "should handle simple variable usage via file" $ do
        result <- runGladosFile "test.scm"
        result `shouldBe` Right "43"

      it "should handle function definitions via file" $ do
        result <- runGladosFile "examplePDF.scm"
        result `shouldBe` Right "3628800"

    describe "Stdin Input Tests" $ do
      it "should handle arithmetic via stdin" $ do
        result <- runGladosStdin "(+ 10 5)\n(- 20 8)\n(* 6 7)\n(div 15 3)\n(mod 17 5)"
        result `shouldBe` Right "2"

      it "should handle booleans via stdin" $ do
        result <- runGladosStdin "(eq? 5 5)\n(eq? 3 7)\n(< 2 8)\n(< 10 5)"
        result `shouldBe` Right "#f"

      it "should handle conditionals via stdin" $ do
        result <- runGladosStdin "(if #t 100 200)\n(if #f 100 200)\n(if (< 3 5) 42 0)"
        result `shouldBe` Right "42"

      it "should handle variables via stdin" $ do
        result <- runGladosStdin "(define a 10)\n(define b 20)\n(+ a b)"
        result `shouldBe` Right "30"

      it "should handle nested expressions via stdin" $ do
        result <- runGladosStdin "(define x 5)\n(* (+ x 2) (- x 1))"
        result `shouldBe` Right "28"

      it "should handle simple operations via stdin" $ do
        result <- runGladosStdin "(define x 42)\n(+ x 1)"
        result `shouldBe` Right "43"

    describe "Error Handling" $ do
      it "should exit with code 84 on parse errors (file)" $ do
        (exitCode, _, _) <- readProcessWithExitCode "./glados" ["nonexistent.scm"] ""
        exitCode `shouldBe` ExitFailure 84

      it "should exit with code 84 on parse errors (stdin)" $ do
        (exitCode, _, _) <- readProcessWithExitCode "./glados" [] "(invalid syntax"
        exitCode `shouldBe` ExitFailure 84