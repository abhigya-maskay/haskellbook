-- [[file:heavy-lifting.org::*1)][1):1]]
a = fmap (+ 1) $ read "[1]" :: [Int]
-- 1):1 ends here

-- [[file:heavy-lifting.org::*2)][2):1]]
b = fmap (++ "lol") <$> Just ["Hi,", "Hello"]
-- 2):1 ends here

-- [[file:heavy-lifting.org::*3)][3):1]]
c = (*2) . (\x -> x - 2)
-- 3):1 ends here

-- [[file:heavy-lifting.org::*4)][4):1]]
d =
  fmap ((return '1' ++) . show)
  (\x -> [x, 1..3])
-- 4):1 ends here

-- [[file:heavy-lifting.org::*5)][5):1]]
e :: IO Integer
e = let
  ioi = readIO "1" :: IO Integer
  changed = read . ("123"++) . show <$> ioi
  in
  fmap (*3) changed
-- 5):1 ends here
