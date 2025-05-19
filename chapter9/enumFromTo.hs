eft :: (Eq a, Enum a) => a -> a -> [a]
eft f t
  | fromEnum f > fromEnum t = []
  | otherwise = go f t []
  where
    go f t acc
      | f == t = acc
      | otherwise = go (succ f) t (f:acc)

eftBool :: Bool -> Bool -> [Bool]
eftBool = eft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft
