module Parser.Lexer where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

type Parser = Parsec Void String

-- | Space consumer: skips spaces, newlines, and comments
whitespace :: Parser ()
whitespace = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespace

symbol :: String -> Parser String
symbol = L.symbol whitespace

-- symbols
------------------------------------------------------------

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

semi :: Parser String
semi = symbol ";"

comma :: Parser String
comma = symbol ","


-- Key words and Identifiers
------------------------------------------------------------

-- keywords, ne peuvent pas etre des identifiants
keywords :: [String]
keywords =
  [ "if", "else", "while", "return"
  , "int", "bool", "void"
  , "true", "false"
  ]

-- Parse an identifier (letter followed by letters/digits/_)
pIdentifier :: Parser String
pIdentifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> identStart <*> many identChar
    identStart = letterChar <|> char '_'
    identChar = alphaNumChar <|> char '_'
    check x = if x `elem` keywords
              then fail $ "keyword " ++ show x ++ " cannot be an identifier"
              else return x

-- Literals
------------------------------------------------------------

-- | Parse an integer literal
pNumber :: Parser Int
pNumber = lexeme L.decimal

-- Parse a boolean
pBool :: Parser Bool
pBool =  (True <$ symbol "true")
     <|> (False <$ symbol "false")
