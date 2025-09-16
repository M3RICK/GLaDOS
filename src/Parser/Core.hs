module Parser.Core(parseExpr, readExpr) where

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

-- Entry point
readExpr::String -> Either ParseError LispVal
readExpr input =
  parse (whitespace *> parseExpr <* eof) "<stdin>" input
