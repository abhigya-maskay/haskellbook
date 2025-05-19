-- [[file:chapter-exercises.org::Cipher][Cipher]]
module Cipher where

import Data.Char

charWidth :: Int
charWidth = 26

getLeftAnchor :: Int -> Int
getLeftAnchor x = if isUpper (chr x) then ord 'A' else ord 'a'

shiftRightInt :: Int -> Int -> Int
shiftRightInt shifter charInt = (charInt + shifter - leftAnchor) `mod` charWidth + leftAnchor
  where
    leftAnchor = getLeftAnchor charInt

shiftRight :: Int -> Char -> Char
shiftRight shifter x =
  if isAlpha x
    then chr . shiftRightInt shifter . ord $ x
    else x

caesar :: Int -> String -> String
caesar shifter = map (shiftRight shifter)

uncaesar :: Int -> String -> String
uncaesar shifter = map reverseShift
  where
    reverseShift = shiftRight (-shifter)
-- Cipher ends here
