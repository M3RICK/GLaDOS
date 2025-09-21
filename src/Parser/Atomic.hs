module Parser.Atomic(parseNumber, parseBool, parseSymbol) where

import Text.Parsec
import Text.Parsec.String(Parser)
import Types
import Parser.Helpers(lexeme)

parseNumber :: Parser LispVal
parseNumber = lexeme $ do
  sign <- optionMaybe (oneOf "+-")
  digits <- many1 digit
  let n = read digits
  return $ Number (case sign of
                     Just '-' -> -n
                     _        -> n)

parseBool :: Parser LispVal
parseBool = lexeme $ do
  _ <- char '#'
  b <- oneOf "tf"
  return $ Bool(b == 't')

parseSymbol :: Parser LispVal
parseSymbol = lexeme $ do
  premier <- letter <|> oneOf "+-*/=<>?!"
  r <- many(letter <|> digit <|> oneOf "+-*/=<>?!")
  return $ Atom(premier:r)
