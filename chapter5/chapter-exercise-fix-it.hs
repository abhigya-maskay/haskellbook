-- 1)
module Sing where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if (y > x) then
  fstString x
  else
  sndString y
  where
    x = "Singing"
    y = "Somewhere"
