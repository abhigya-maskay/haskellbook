module Integer where

import Data.Char (isDigit)
import Data.Attoparsec.Text
import Data.Maybe (isJust)
import Control.Applicative (optional)

parseDigit :: Parser Char
parseDigit = satisfy (isDigit)

base10Integer :: Parser Integer
base10Integer = read <$> many1 parseDigit

base10Integer' :: Parser Integer
base10Integer' = do
  sign <- optional . char $ '-'
  integer <- base10Integer
  return (if isJust sign then negate integer else integer)
