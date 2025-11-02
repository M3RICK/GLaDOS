module Pointer (checkPointers, testPointerRejection) where

import Data.Char (isAlphaNum)
import Data.List (isInfixOf)

checkPointers :: String -> Either String ()
checkPointers src
  | any (`isInfixOf` src) forbiddenPatterns =
      Left "SecurityError: pointer manipulation is not allowed in glados."
  | otherwise = Right ()
  where
    forbiddenPatterns =
      [ "*", "&", "ptr", "pointer", "ref", "deref" ]

testPointerRejection :: IO ()
testPointerRejection = do
  let safeCode   = "(define x 42)"
      unsafeCode = "(define ptrX (* &x))"
  putStrLn "Test"
  print $ checkPointers safeCode
  print $ checkPointers unsafeCode
