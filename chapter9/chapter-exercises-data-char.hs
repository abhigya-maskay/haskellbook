-- [[file:chapter-exercises.org::Imports][Imports]]
import Data.Char
-- Imports ends here

-- [[file:chapter-exercises.org::Two][Two]]
filterUpper :: String -> String
filterUpper = filter isUpper
-- Two ends here

-- [[file:chapter-exercises.org::Three][Three]]
titlize :: String -> String
titlize "" = ""
titlize (x:xs) = (toUpper x):xs
-- Three ends here

-- [[file:chapter-exercises.org::Four][Four]]
capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = (toUpper x):(capitalize xs)
-- Four ends here

-- [[file:chapter-exercises.org::Five][Five]]
firstUpper :: String -> Char
firstUpper = toUpper . head
-- Five ends here
