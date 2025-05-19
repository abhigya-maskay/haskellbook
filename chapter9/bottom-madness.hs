-- [[file:bottom-madness.org::One][One]]
one = [x ^ y | x <- [1 .. 5], y <- [2, undefined]]

-- One ends here

-- [[file:bottom-madness.org::Two][Two]]
two = take 1 $ [x ^ y | x <- [1 .. 5], y <- [2, undefined]]

-- Two ends here

-- [[file:bottom-madness.org::Three][Three]]
three = sum [1, undefined, 3]

-- Three ends here

-- [[file:bottom-madness.org::Four][Four]]
four = length [1, 2, undefined]

-- Four ends here

-- [[file:bottom-madness.org::Five][Five]]
five = length $ [1, 2, 3] ++ undefined

-- Five ends here

-- [[file:bottom-madness.org::Six][Six]]
six = take 1 $ filter even [1, 2, 3, undefined]

-- Six ends here

-- [[file:bottom-madness.org::Seven][Seven]]
seven = take 1 $ filter even [1, 3, undefined]

-- Seven ends here

-- [[file:bottom-madness.org::Eight][Eight]]
eight = take 1 $ filter odd [1, 3, undefined]

-- Eight ends here

-- [[file:bottom-madness.org::Nine][Nine]]
nine = take 2 $ filter odd [1, 3, undefined]

-- Nine ends here

-- [[file:bottom-madness.org::Ten][Ten]]
ten = take 3 $ filter odd [1, 3, undefined]

-- Ten ends here
