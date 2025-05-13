-- [[file:chapter-exercises.org::FDOne][FDOne]]
data DividedResult a where
  Result :: a -> a -> DividedResult a
  DividedByZero :: DividedResult a
  deriving (Show)

dividedBy :: (Integral a) => a -> a -> DividedResult a
dividedBy _ 0 = DividedByZero
dividedBy num denom = res
  where
    res = Result quot rem
    (quot, rem) = go num denom 0
    negateQuot (x, y) = (-x, y)
    go n d count
      | n < 0 && d < 0 = negateQuot $ go (-n) (-d) count
      | n < 0 = negateQuot $ go (-n) d count
      | d < 0 = negateQuot $ go n (-d) count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)
-- FDOne ends here
