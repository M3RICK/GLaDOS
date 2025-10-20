-- src/Parser/Program.hs
module Parser.Program (pProgram) where

import Parser.Function
import Parser.Lexer (Parser)  -- Add this import
import AST.AST
import Text.Megaparsec

pProgram :: Parser Program
pProgram = Program <$> some pFunction
