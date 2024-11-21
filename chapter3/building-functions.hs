fa :: String -> String
fa x = x ++ "!"

a = fa "Curry is awesome"

fb :: String -> Char
fb x = x !! 4

b = fb "Curry is awesome!"

fc :: String -> String
fc x = drop 9 x

c = fc "Curry is awesome!"

thirdLetter :: String -> Char
thirdLetter x = x !! 3

letterIndex :: Int -> Char
letterIndex i = "Curry is awesome!" !! i

