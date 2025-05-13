-- [[file:chapter-exercises.org::McOne][McOne]]
mc91 :: (Integral a) => a -> a
mc91 n
  | n > 100 = n - 10
  | n <= 100 = mc91 (mc91 (n + 11))
-- McOne ends here
