module Parser.Core (parseProgram) where

import Parser.Program
import Parser.Lexer
import AST.AST
import Text.Megaparsec
import Data.Void

parseProgram :: String -> Either (ParseErrorBundle String Void) Program
parseProgram input = parse (whitespace *> pProgram <* eof) "input" input
