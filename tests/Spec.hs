{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-TLS-5-1-glados-2 [WSL: Ubuntu-22.04]
-- File description:
-- Spec
-}

-- Spec file for manual import (temporary fix for VS Code HLS)
module Main (main) where

import Test.Hspec
import qualified ParserSpec
import qualified TypeCheckerSpec  
import qualified VirtualMachineSpec
import qualified CompilerSpec

main :: IO ()
main = hspec $ do
  describe "Parser" ParserSpec.spec
  describe "TypeChecker" TypeCheckerSpec.spec
  describe "VirtualMachine" VirtualMachineSpec.spec
  describe "Compiler" CompilerSpec.spec