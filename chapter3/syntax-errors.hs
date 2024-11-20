-- 1. Does not compile because operator needs to have parenthesis
x = (++) [1,2,3] [4,5,6]
-- 2. Does not compile because the quotes need to be different for string
y = "<3" ++ " Haskell"
-- 3. Compiles
z = concat ["<3", " Haskell"]
