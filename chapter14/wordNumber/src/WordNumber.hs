module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = case n of
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"
  0 -> "zero"

digits :: Int -> [Int]
digits n = go n []
  where
    go 0 acc = acc
    go n' acc = go initDigits (lastDigit:acc)
      where
        initDigits = div n' 10
        lastDigit = mod n' 10

wordNumber :: Int -> String
wordNumber n = concat . intersperse "-" . map digitToWord . digits $ n
-- N2W ends here
