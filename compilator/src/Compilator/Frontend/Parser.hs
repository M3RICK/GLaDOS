module Compilator.Frontend.Parser (parseProgram) where

import Compilator.Frontend.AST
import Data.Void
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") empty

lexeme  :: Parser a -> Parser a
lexeme  = L.lexeme sc
symbol  :: String -> Parser String
symbol  = L.symbol sc
kw :: String -> Parser ()
kw w    = void (symbol w)

reserved :: [String]
reserved = ["let","func","print"]

ident :: Parser Name
ident = (lexeme . try) $ do
  x <- (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
  if x `elem` reserved then fail "reserved" else pure x

integer :: Parser Integer
integer = lexeme (L.signed sc L.decimal)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pPrimary :: Parser Expr
pPrimary =
      (ELitInt <$> integer)
  <|> try pCall
  <|> (EVar <$> ident)
  <|> parens pExpr

pCall :: Parser Expr
pCall = do
  f <- ident
  args <- parens (pExpr `sepBy` symbol ",")
  pure (ECall f args)

pExpr :: Parser Expr
pExpr = makeExprParser pPrimary
  [ [ InfixL (EBin Mul <$ symbol "*") ]
  , [ InfixL (EBin Add <$ symbol "+") ]
  ]

pDecl :: Parser Decl
pDecl =
      (do kw "let"; n <- ident; _ <- symbol "="; e <- pExpr; pure (DLet n e))
  <|> (do kw "func"; n <- ident
          ps <- parens (ident `sepBy` symbol ",")
          _ <- symbol "="
          body <- pExpr
          pure (DFunc n ps body))

pStmt :: Parser Stmt
pStmt = do kw "print"; e <- parens pExpr; pure (SPrint e)

pTop :: Parser Top
pTop = (TDecl <$> pDecl) <|> (TStmt <$> pStmt)

pProgram :: Parser Program
pProgram = sc *> (Program <$> many pTop) <* eof

parseProgram :: String -> Either String Program
parseProgram s = case runParser pProgram "<source>" s of
  Left e  -> Left (errorBundlePretty e)
  Right p -> Right p
