module Parser.Helpers where

import Text.Parsec
import Text.Parsec.String(Parser)

whitespace::Parser()
whitespace = skipMany (oneOf " \t\n\r")

lexeme::Parser a -> Parser a
lexeme p = p <* whitespace

parens::Parser a -> Parser a
parens = between(lexeme (char '(')) (lexeme (char ')'))
