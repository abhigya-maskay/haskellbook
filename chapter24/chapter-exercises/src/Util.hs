module Util where

import Data.Maybe (listToMaybe)

chunk :: Int -> [a] -> [[a]]
chunk n list = go n list []
  where
    go :: Int -> [a] -> [[a]] -> [[a]]
    go _ [] acc = reverse acc
    go n' list' acc = go n' (drop n' list') (take n' list' : acc)

leftPad :: Int -> String -> String
leftPad desiredLength str
  | desiredLength <= length str = str
  | otherwise = replicate (desiredLength - length str) '0' ++ str

readx :: (String -> [(Integer, String)]) -> String -> Integer
readx reader = maybe 0 fst . listToMaybe . reader

showx :: (Integral a) => (a -> (String -> String)) -> a -> String
showx shower = ($ "") . shower
