module Parser.Lexer where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import AST.AST (Located(..))

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

-- Helper to wrap parsed values with source position
located :: Parser a -> Parser (Located a)
located p = do
  pos <- getSourcePos
  val <- p
  return (Located pos val)

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

-- Parse an identifier (returns Located String for position tracking)
pIdentifier :: Parser (Located String)
pIdentifier = located $ (lexeme . try) (p >>= check)
  where
    p = (:) <$> identStart <*> many identChar
    identStart = letterChar <|> char '_'
    identChar = alphaNumChar <|> char '_'
    check x = if x `elem` keywords
              then fail $ "keyword " ++ show x ++ " cannot be an identifier"
              else return x

-- Literals
------------------------------------------------------------

-- Parse an integer literal with position
pNumber :: Parser (Located Int)
pNumber = located (lexeme L.decimal)

-- Parse a boolean with position
pBool :: Parser (Located Bool)
pBool = located $
      (True <$ symbol "true")
  <|> (False <$ symbol "false")
