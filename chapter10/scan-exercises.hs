fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibs20 = take 20 fibs

fibsLessThan100 = takeWhile (< 100) fibs

factorial = scanl (*) 1 [2..]
