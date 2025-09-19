module Parser.Helpers (whitespace, lexeme, parens) where
import Text.Parsec
import Text.Parsec.String(Parser)

whitespace::Parser ()
whitespace = skipMany (spaceChar <|> comment)
  where
    spaceChar = oneOf " \t\n\r" >> return ()
    comment   = char ';' >> skipMany (noneOf "\n") >> return ()

lexeme::Parser a -> Parser a
lexeme p = p <* whitespace

parens::Parser a -> Parser a
parens = between(lexeme (char '(')) (lexeme (char ')'))
