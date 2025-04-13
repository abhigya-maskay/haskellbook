-- 1)
myConcat x = x +++ " yo"
-- myConcat has type [Char] -> [Char] because the second argument we apply it to is a String

-- 2)
myMult x = (x / 3) * 5
-- myMult will have type Fractional a => a -> a because the argument being used with the / operator implies that it is fractional

-- 3)
myTake x = take x "hey you"
-- myTake has a type of Int -> String because the second argument to take is of type String

-- 4)
myCom x = x > (length [1..10])
-- myCom has a type of Int -> Bool because the second argument to the > operator is already of type Int

-- 5)
myAlph x = x < 'z'
-- myAlph has a type of Char -> Bool because the second argument to the > operator is already of the type Char
