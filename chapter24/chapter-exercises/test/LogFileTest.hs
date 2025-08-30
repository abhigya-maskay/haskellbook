{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LogFileTest where

import Data.Attoparsec.Text
import qualified Data.Text as T
import Data.Time
import LogFile
import Test.Hspec
import Test.QuickCheck
import TestUtil (parseResultShouldBe, parseResultShouldError)
import Text.RawString.QQ

exampleComment :: T.Text
exampleComment = "---- This is a comment"

exampleHeader :: T.Text
exampleHeader = "# 2025-01-02"

exampleHeaderDate :: Date
exampleHeaderDate = fromGregorian 2025 01 02

exampleHeaderWithSpace :: T.Text
exampleHeaderWithSpace = "# 2025-01-02 -- It should skip this comment"

exampleMalformedHeader :: T.Text
exampleMalformedHeader = "# 25-012"

exampleEntry :: T.Text
exampleEntry = "8:00 Breakfast"

exampleParsedTimeEntry :: TimeEntry
exampleParsedTimeEntry = TimeEntry (TimeOfDay 8 0 0) "Breakfast"

exampleMalformedEntry :: T.Text
exampleMalformedEntry = "0800 Breakfast"

exampleEntryWithComment :: T.Text
exampleEntryWithComment = "08:00 Breakfast-- trying to see if this skips comments"

exampleEntryWithSpace :: T.Text
exampleEntryWithSpace = "08:00 Breakfast "

exampleSection :: T.Text
exampleSection =
  [r|
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Coming home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep
|]

exampleParsedSection :: DayEntry
exampleParsedSection =
  DayEntry
    (fromGregorian 2025 2 5)
    [ TimeEntry (TimeOfDay 8 0 0) "Breakfast",
      TimeEntry (TimeOfDay 9 0 0) "Sanitizing moisture collector",
      TimeEntry (TimeOfDay 11 0 0) "Exercising in high-grav gym",
      TimeEntry (TimeOfDay 12 0 0) "Lunch",
      TimeEntry (TimeOfDay 13 0 0) "Programming",
      TimeEntry (TimeOfDay 17 0 0) "Coming home in rover",
      TimeEntry (TimeOfDay 17 30 0) "R&R",
      TimeEntry (TimeOfDay 19 0 0) "Dinner",
      TimeEntry (TimeOfDay 21 0 0) "Shower",
      TimeEntry (TimeOfDay 21 15 0) "Read",
      TimeEntry (TimeOfDay 22 0 0) "Sleep"
    ]

exampleLogFile :: T.Text
exampleLogFile = T.concat [initialComment, exampleSection, secondSection]
  where
    initialComment = "-- wheee a comment\n"
    secondSection =
      [r|
# 2025-02-07 -- dates not necessarily sequential
08:00 Breakfast -- should I try skipping bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

exampleParsedSecondSection :: DayEntry
exampleParsedSecondSection =
  DayEntry
    (fromGregorian 2025 2 7)
    [ TimeEntry (TimeOfDay 8 0 0) "Breakfast",
      TimeEntry (TimeOfDay 9 0 0) "Bumped head, passed out",
      TimeEntry (TimeOfDay 13 36 0) "Wake up, headache",
      TimeEntry (TimeOfDay 13 37 0) "Go to medbay",
      TimeEntry (TimeOfDay 13 40 0) "Patch self up",
      TimeEntry (TimeOfDay 13 45 0) "Commute home for rest",
      TimeEntry (TimeOfDay 14 15 0) "Read",
      TimeEntry (TimeOfDay 21 0 0) "Dinner",
      TimeEntry (TimeOfDay 21 15 0) "Read",
      TimeEntry (TimeOfDay 22 0 0) "Sleep"
    ]

exampleParsedLogFile :: LogFile
exampleParsedLogFile = LogFile [exampleParsedSection, exampleParsedSecondSection]

prop_roundTrip :: LogFile -> Bool
prop_roundTrip lf = (parseLogFile . show $ lf) `parseResultEqual` lf
  where
    parseLogFile :: String -> Either String LogFile
    parseLogFile = parseOnly parseLog . T.pack
    parseResultEqual :: Either String LogFile -> LogFile -> Bool
    parseResultEqual parseResult lf' = case parseResult of
      Left _ -> False
      Right lf'' -> lf' == lf''

testLogFile :: IO ()
testLogFile = hspec $ do
  describe "skipComment" $ do
    it "skips a comment" $ do
      parseResultShouldBe skipComment exampleComment ()
  describe "parseHeader" $ do
    it "parses a valid header" $ do
      parseResultShouldBe parseHeader exampleHeader exampleHeaderDate
    it "should fail on a malformed header" $ do
      parseResultShouldError parseHeader exampleMalformedHeader
    it "should parse a header with a comment" $ do
      parseResultShouldBe parseHeader exampleHeaderWithSpace exampleHeaderDate
  describe "parseTimeEntry" $ do
    it "should parse a time entry" $ do
      parseResultShouldBe parseTimeEntry exampleEntry exampleParsedTimeEntry
    it "should parse a time entry with comments at the end" $ do
      parseResultShouldBe parseTimeEntry exampleEntryWithComment exampleParsedTimeEntry
    it "should error on a malformed time entry" $ do
      parseResultShouldError parseTimeEntry exampleMalformedEntry
    it "should strip whitespace from the activity" $ do
      parseResultShouldBe parseTimeEntry exampleEntryWithSpace exampleParsedTimeEntry
  describe "parseSection" $ do
    it "should correctly parse the section" $ do
      parseResultShouldBe parseDayEntry exampleSection exampleParsedSection
  describe "parseLog" $ do
    it "should correctly parse the entire logFile" $ do
      parseResultShouldBe parseLog exampleLogFile exampleParsedLogFile
  describe "roundTrip" $ do
    it "should successfully round trip random log files" $ do
      quickCheck prop_roundTrip
