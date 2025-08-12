module TryTry where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

data FractionOrDecimal = Fraction !Rational
  | Decimal !Double
  deriving (Eq, Show)

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

parseDecimal :: Parser Double
parseDecimal = do
  firstHalf <- decimal
  _ <- char '.'
  secondHalf <- decimal
  let lengthSecond = length . show $ secondHalf
      decimalHalf = fromIntegral secondHalf / (10 ^ lengthSecond)
      decimalNumber = fromIntegral firstHalf + decimalHalf
  return decimalNumber

parseDecimalOrFraction :: String -> Result FractionOrDecimal
parseDecimalOrFraction = parseString parser mempty where
  parser = Fraction <$> try parseFraction <|> Decimal <$> parseDecimal
