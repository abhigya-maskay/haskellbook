import Data.Time

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate
      ( UTCTime
          (fromGregorian 1911 5 1)
          (secondsToDiffTime 34123)
      ),
    DbNumber 9001,
    DbString "Hello, world!",
    DbDate
      ( UTCTime
          (fromGregorian 1921 5 1)
          (secondsToDiffTime 34123)
      )
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = map getDate . filter isDbDate
  where
    isDbDate (DbDate _) = True
    isDbDate _ = False
    getDate (DbDate utcDate) = utcDate

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = map getInteger . filter isDbNumber
  where
    isDbNumber (DbNumber _) = True
    isDbNumber _ = False
    getInteger (DbNumber int) = int

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb xs = sumNum / lenNum
  where
    sumNum = fromIntegral . sumDb $ xs
    lenNum = fromIntegral . length . filterDbNumber $ xs
