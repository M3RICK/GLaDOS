module Parser.Compound(parseList) where

import Text.Parsec
import Text.Parsec.String(Parser)
import Types
import Parser.Helpers(parens, whitespace)
import Parser.Atomic(parseNumber, parseBool, parseSymbol)

-- parseExpr dans Core.hs → faut resoudre le bordel avec une injection de param plus tard.
parseList::Parser LispVal -> Parser LispVal
parseList parseExpr =
  List <$> parens (parseExpr `sepEndBy` whitespace)
