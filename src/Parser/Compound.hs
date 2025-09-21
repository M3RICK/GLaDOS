module Parser.Compound(parseList) where
import Text.Parsec
import Text.Parsec.String(Parser)
import Types
import Parser.Helpers(parens, whitespace)

parseList :: Parser LispVal -> Parser LispVal
parseList parseExpr =
  List <$> parens (parseExpr `sepEndBy` whitespace)
