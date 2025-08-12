module SemVer where

import Control.Applicative
import Data.Attoparsec.Text
import Prelude hiding (takeWhile)
import Data.Foldable (fold)

data NumberOrString
  = NOSS !String
  | NOSI !Integer
  deriving (Show, Eq, Ord)

type Major = Integer

type Minor = Integer

type Patch = Integer

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer = SemVer !Major !Minor !Patch !Release !Metadata deriving (Show, Eq)

instance Ord SemVer where
  compare sv sv' = compareVersionOnly sv sv' <> compareNonVersionOnly sv sv'

compareNumberOrString :: NumberOrString -> NumberOrString -> Ordering
compareNumberOrString (NOSI n) (NOSI n') = compare n n'
compareNumberOrString (NOSI _) (NOSS _) = LT
compareNumberOrString (NOSS _) (NOSI _) = GT
compareNumberOrString (NOSS s) (NOSS s') = compare s s'

compareNumbersOrStrings :: [NumberOrString] -> [NumberOrString] -> Ordering
compareNumbersOrStrings ns = fold . zipWith compareNumberOrString ns

compareVersionOnly :: SemVer -> SemVer -> Ordering
compareVersionOnly (SemVer ma mi p _ _) (SemVer ma' mi' p' _ _) =
  compare ma ma' <> compare mi mi' <> compare p p'

compareNonVersionOnly :: SemVer -> SemVer -> Ordering
compareNonVersionOnly (SemVer _ _ _ [] []) (SemVer _ _ _ [] []) = EQ
compareNonVersionOnly (SemVer _ _ _ [] []) (SemVer _ _ _ _ _) = GT
compareNonVersionOnly (SemVer _ _ _ _ _) (SemVer _ _ _ [] []) = LT
compareNonVersionOnly (SemVer _ _ _ r m) (SemVer _ _ _ r' m') = compareNumbersOrStrings r r' <> compareNumbersOrStrings m m'

parseNOSSChar :: Parser Char
parseNOSSChar = letter <|> digit <|> char '-'

parseNOSI :: Parser NumberOrString
parseNOSI = NOSI <$> decimal <* endOfInput

parseNOSS :: Parser NumberOrString
parseNOSS = NOSS <$> many1 parseNOSSChar

parseNumberOrString :: Parser [NumberOrString]
parseNumberOrString = (parseNOSI <|> parseNOSS) `sepBy` char '.'

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- decimal
  _ <- char '.'
  minor <- decimal
  _ <- char '.'
  patch <- decimal
  _ <- optional . char $ '-'
  release <- parseNumberOrString
  _ <- optional . char $ '+'
  metadata <- parseNumberOrString
  return (SemVer major minor patch release metadata)
