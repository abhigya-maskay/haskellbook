module PhoneNumber where

import Control.Applicative (optional, (<|>))
import Data.Attoparsec.Text
import Prelude hiding (take)

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
  _ <- optional . char $ '('
  _ <- optional (digit <* char '-')
  numberingPlanArea <- fmap read . count 3 $ digit
  _ <- optional (char ')' <|> char '-')
  _ <- optional (char ' ')
  exchange <- fmap read . count 3 $ digit
  _ <- optional (char '-')
  lineNumber <- fmap read . count 4 $ digit
  return $ PhoneNumber numberingPlanArea exchange lineNumber
