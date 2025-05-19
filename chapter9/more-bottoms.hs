-- [[file:more-bottoms.org::Import][Import]]
import Data.Bool (bool)
-- Import ends here

-- [[file:more-bottoms.org::One][One]]
one = take 1 $ map (+1) [undefined, 2, 3]
-- One ends here

-- [[file:more-bottoms.org::Two][Two]]
two = take 1 $ map (+1) [1, undefined, 3]
-- Two ends here

-- [[file:more-bottoms.org::Three][Three]]
three = take 2 $ map (+1) [1, undefined, 3]
-- Three ends here

-- [[file:more-bottoms.org::Four][Four]]
itIsMystery :: String -> [Bool]
itIsMystery xs = map (\x -> elem x "aeiou") xs
-- Four ends here

-- [[file:more-bottoms.org::Five][Five]]
fiveA = map (^2) [1..10]
fiveB = map minimum [[1..10], [10..20], [20..30]]
fiveC = map sum [[1..5], [1..5], [1..5]]
-- Five ends here

-- [[file:more-bottoms.org::Six][Six]]
six = map (\x -> bool x (-x) (x == 3)) [1..10]
-- Six ends here
