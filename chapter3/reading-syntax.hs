-- No errors
a = concat [[1,2,3], [4,5,6]]
-- Needs parenthesis
b = (++) [1,2,3] [4,5,6]
-- No errors
c = (++) "hello" " world"
-- Needs to have the quote properly closed
d = ["hello" ++ " world"]
-- Argument order is reversed
e = "hello" !! 4
-- No errors
f = (!!) "hello" 4
-- Int argument needs to be outside the quotes
g = take 4 "lovely"
-- no errors
h = take 3 "awesome"
