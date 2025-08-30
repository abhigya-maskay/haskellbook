{-# LANGUAGE OverloadedStrings #-}

module LogFile where

import Data.Time
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Control.Applicative
import Control.Monad
import Data.Text hiding (length, replicate, concat, intercalate)
import Data.List (intercalate)
import Test.QuickCheck
import Util (leftPad)

type Activity = Text
type Time = TimeOfDay
type Date = Day
data TimeEntry = TimeEntry !Time !Activity deriving (Eq)
data DayEntry = DayEntry !Date ![TimeEntry] deriving (Eq)
newtype LogFile = LogFile [DayEntry] deriving (Eq)

instance Show TimeEntry where
  show (TimeEntry time activity) = showTime ++ showActivity where
    showTime = (leftPad 2 . show . todHour $ time) ++ ":" ++ (leftPad 2 . show . todMin $ time) ++ " "
    showActivity = unpack activity

removeNewLines :: Text -> Text
removeNewLines = Data.Text.filter (not . isEndOfLine)

instance Arbitrary TimeEntry where
  arbitrary = do
    hour <- chooseInt (0,23)
    minute <- chooseInt (0,59)
    activity <- fmap (removeNewLines . pack) . resize 50 . listOf1 . elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    return $ TimeEntry (TimeOfDay hour minute 0) activity

instance Ord TimeEntry where
  (TimeEntry t1 _) `compare` (TimeEntry t2 _) = t1 `compare` t2

instance Show DayEntry where
  show (DayEntry date timeEntries) = showDate ++ "\n" ++ showTimeEntries where
    showDate = "# " ++ showGregorian date
    showTimeEntries = intercalate "\n" . fmap show $ timeEntries

instance Arbitrary DayEntry where
  arbitrary = do
    year <- chooseInteger (2000,2025)
    month <- chooseInt (0,12)
    day <- chooseInt (1,28)
    timeEntries <- resize 20 orderedList
    return $ DayEntry (fromGregorian year month day) timeEntries

instance Show LogFile where
  show (LogFile dayEntries) = intercalate "\n\n" . fmap show $ dayEntries

instance Arbitrary LogFile where
  arbitrary = fmap LogFile . resize 10 . listOf $ arbitrary

parseCommentStart :: Parser Text
parseCommentStart = string "--"

skipRestOfLine :: Parser ()
skipRestOfLine = skipWhile (not . isEndOfLine)

skipComment :: Parser ()
skipComment = skipMany1 parseCommentStart <* skipRestOfLine

skipCommentAndSpaces :: Parser ()
skipCommentAndSpaces = do
  _ <- optional skipSpace
  _ <- optional skipComment
  _ <- optional skipSpace
  return ()

parseHeader :: Parser Day
parseHeader = do
  _ <- char '#'
  _ <- skipSpace
  year <- decimal
  _ <- char '-'
  month <- decimal
  _ <- char '-'
  day <- decimal
  _ <- skipCommentAndSpaces
  return $ fromGregorian year month day

parseCommentOrEndOfLine :: Parser ()
parseCommentOrEndOfLine = void parseCommentStart <|> endOfLine <|> endOfInput

parseTimeEntry :: Parser TimeEntry
parseTimeEntry = do
  _ <- optional skipSpace
  hour <- decimal
  _ <- char ':'
  minutes <- decimal
  _ <- skipSpace
  activity <- manyTill anyChar (lookAhead parseCommentOrEndOfLine)
  _ <- skipCommentAndSpaces
  return $ TimeEntry (TimeOfDay hour minutes 0) (strip . pack $ activity)

parseDayEntry :: Parser DayEntry
parseDayEntry = do
  _ <- skipCommentAndSpaces
  date <- parseHeader
  entries <- many' parseTimeEntry
  _ <- skipCommentAndSpaces
  return $ DayEntry date entries

parseLog :: Parser LogFile
parseLog = LogFile <$> many' parseDayEntry
