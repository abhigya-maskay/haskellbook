-- [[file:square-cube.org::One][One]]
mySqr = [x ^ 2 | x <- [1 .. 5]]

myCube = [y ^ 3 | y <- [1 .. 5]]

tuples = [(s, c) | s <- mySqr, c <- myCube]

-- One ends here

-- [[file:square-cube.org::Two][Two]]
tuples' = [(s, c) | s <- mySqr, c <- myCube, s < 50, c < 50]

-- Two ends here

-- [[file:square-cube.org::Three][Three]]
tupleLength = length [(s, c) | s <- mySqr, c <- myCube, s < 50, c < 50]

-- Three ends here
