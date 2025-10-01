module Parser.Expr where

import Parser.Lexer
import AST.AST

import Control.Monad (void)
import Text.Megaparsec
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L

-- Expression parser
------------------------------------------------------------

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

-- | Parse atomic expressions
pTerm :: Parser Expr
pTerm =
      (BoolLit <$> pBool)          -- true/false
  <|> (NumLit <$> pNumber)         -- numbers
  <|> try pCall                    -- function call
  <|> (Var <$> pIdentifier)        -- variable
  <|> parens pExpr                 -- parenthesized expression

-- | Parse function calls
pCall :: Parser Expr
pCall = do
  name <- pIdentifier
  args <- parens (pExpr `sepBy` comma)
  return (Call name args)

-- Operator precedence
------------------------------------------------------------

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ binary "*" (BinOp Mul), binary "/" (BinOp Div) ]
  , [ binary "+" (BinOp Add), binary "-" (BinOp Sub) ]
  , [ binary "<=" (BinOp Le), binary ">=" (BinOp Ge)
    , binary "==" (BinOp Eq), binary "!=" (BinOp Neq)
    , binary "<" (BinOp Lt), binary ">" (BinOp Gt) ]
  , [ binary "&&" (BinOp And) ]
  , [ binary "||" (BinOp Or) ]
  ]

-- Modified binary operator to handle Located values
binary :: String -> (Located Expr -> Located Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL $ do
  void (try (symbol name))
  pos <- getSourcePos  -- on choppe la position quand on detecte un op
  return $ \e1 e2 ->
        f (Located pos e1) (Located pos e2)
