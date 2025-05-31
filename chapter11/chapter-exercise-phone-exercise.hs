-- [[file:chapter-exercises.org::*1)][1):1]]
import Data.Char (isUpper, toLower)
import Data.List (elemIndex, maximumBy, group, sort)
import Data.Ord (comparing)
import Data.Maybe (maybe, maybeToList)

data DaPhone = DaPhone [(Char, String)]

phone =
  DaPhone
    [ ('1', "1"),
      ('2', "abc2"),
      ('3', "def3"),
      ('4', "ghi4"),
      ('5', "jkl5"),
      ('6', "mno6"),
      ('7', "pqrs7"),
      ('8', "tuv8"),
      ('9', "wxyz9"),
      ('0', " 0"),
      ('#', ".,#")
    ]

convo :: [String]
convo =
  [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol OK. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "OK. Do u thing I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"
  ]
-- 1):1 ends here

-- [[file:chapter-exercises.org::*2)][2):1]]
type Digit = Char

type Presses = Int

findKey :: DaPhone -> Char -> (Char, String)
findKey (DaPhone phone) c = head . filter hasChar $ phone
  where
    lowerC = toLower c
    hasChar = elem lowerC . snd

findPresses :: (Char, String) -> Char -> [(Digit, Presses)]
findPresses (p, s) c = if (isUpper c) then shift : charPress else charPress
  where
    lowerC = toLower c
    shift = ('*', 1)
    charPress = map (p,) . maybeToList . fmap (+ 1) . elemIndex lowerC $ s

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone c = findPresses key c
  where
    key = findKey phone c

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone = concat . map (reverseTaps phone)
-- 2):1 ends here

-- [[file:chapter-exercises.org::*3)][3):1]]
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd
-- 3):1 ends here

-- [[file:chapter-exercises.org::*4)][4):1]]
mostPopular :: [a] -> a
mostPopular = head . maximumBy (comparing length) . group . sort

costOfLetter :: Char -> String -> Presses
costOfLetter c s =  fingerTaps . cellPhonesDead phone . filter (== c) $ s

mostPopularAndCost ::  String -> (Char, Presses)
mostPopularAndCost s = (popularLetter, costOfLetter popularLetter s)
  where
    popularLetter = mostPopular s
-- 4):1 ends here

-- [[file:chapter-exercises.org::*5)][5):1]]
coolestLtr :: [String] -> Char
coolestLtr = mostPopular . concat

coolestWord :: [String] -> String
coolestWord = mostPopular
-- 5):1 ends here
