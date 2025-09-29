module Parser.Program where

import Parser.Function
import AST.AST

import Text.Megaparsec


-- Program parser
------------------------------------------------------------

-- A program = list of one or more functions

pProgram :: Parser Program
pProgram = Program <$> some pFunction
