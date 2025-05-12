-- [[file:chapter-exercises.org::*1) a)][1) a):1]]
tensDigit :: (Integral a) => a -> a
tensDigit x = d
  where
    x' = fst . divMod x $ 10
    d = snd . divMod x' $ 10

-- 1) a):1 ends here

-- [[file:chapter-exercises.org::*1) a)][1) a):2]]
hunsD :: (Integral a) => a -> a
hunsD x = d
  where
    x' = fst . divMod x $ 100
    d = snd . divMod x' $ 10

-- 1) a):2 ends here

-- [[file:chapter-exercises.org::*2)][2):1]]
foldBool :: a -> a -> Bool -> a
foldBool x y bool = case bool of
  True -> x
  False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y bool
  | bool = x
  | otherwise = y

-- 2):1 ends here

-- [[file:chapter-exercises.org::*3)][3):1]]
g :: (a -> b) -> (a, c) -> (b, c)
g aToB (a, c) = (aToB a, c)

-- 3):1 ends here

-- [[file:chapter-exercises.org::*4)][4):1]]
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

main = do
  print (roundTrip 4)
  print (id 4)

-- 4):1 ends here

-- [[file:chapter-exercises.org::*5)][5):1]]
roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show

main' = do
  print (roundTrip' 4)
  print (id 4)

-- 5):1 ends here

-- [[file:chapter-exercises.org::*6)][6):1]]
roundTrip'' :: (Show a, Read b) => a -> b
roundTrip'' = read . show

main'' = do
  print ((roundTrip'' :: Int -> Int) 4)
  print (id 4)

-- 6):1 ends here
