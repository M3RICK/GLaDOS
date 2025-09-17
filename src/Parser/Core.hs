module Parser.Core(parseExpr, readExpr, readProgram) where

import Text.Parsec
import Text.Parsec.String(Parser)
import Types
import Parser.Helpers(whitespace)
import Parser.Atomic(parseNumber, parseBool, parseSymbol)
import Parser.Compound(parseList)

-- Dispatcher
parseExpr::Parser LispVal
parseExpr = choice
  [ try parseNumber
  , try parseBool
  , try parseSymbol
  , parseList parseExpr
  ]

-- Entry point (singular)
readExpr::String -> Either ParseError LispVal
readExpr input =
  parse (whitespace *> parseExpr <* eof) "<stdin>" input

-- Entry point (multiple)
readProgram :: String -> Either ParseError [LispVal]
readProgram input =
  parse (whitespace *> many parseExpr <* eof) "<stdin>" input