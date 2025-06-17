module Lib where


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

newtype Keyword = Keyword String deriving (Show, Eq)
newtype Message = Message String deriving (Show, Eq)
newtype EncodedMessage = EncodedMessage String deriving (Show, Eq)

data Paired a b = Skipped b | Paired a b

pair :: String -> String -> [Paired Char Char]
pair k m = reverse . go k m $ []
  where
    go _ "" acc = acc
    go "" m acc = go k m acc
    go keyword@(k:ks) (m:ms) acc
      | isAlpha m = go ks ms ((Paired k m):acc)
      | otherwise = go keyword ms ((Skipped m):acc)

getShift :: Char -> Int
getShift x = ((ord x -) . getLeftAnchor . ord $ x)

getPairShifts :: [Paired Char Char] -> [Paired Int Char]
getPairShifts = map f
  where
    f (Paired x y) = Paired (getShift x) y
    f (Skipped y) = Skipped y

shiftCharRight :: Paired Int Char -> Char
shiftCharRight (Paired s c) = shiftRight s c
shiftCharRight (Skipped c) = c

shiftCharLeft :: Paired Int Char -> Char
shiftCharLeft (Paired s c) = shiftCharRight (Paired (-s) c)
shiftCharLeft p = shiftCharRight p


vignere :: Keyword -> Message -> EncodedMessage
vignere (Keyword k) (Message m) = EncodedMessage . map shiftCharRight . getPairShifts . pair k $ m

unvignere :: Keyword -> EncodedMessage -> Message
unvignere (Keyword k) (EncodedMessage em) = Message . map shiftCharLeft . getPairShifts . pair k $ em
