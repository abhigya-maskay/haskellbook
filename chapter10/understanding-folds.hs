-- [[file:understanding-folds.org::*1)][1):1]]
oneQ = foldr (*) 1 [1..5]
oneA = foldl (flip (*)) 1 [1..5]
-- 1):1 ends here

-- [[file:understanding-folds.org::*5)][5):1]]
fiveA = foldr (++) "" ["woot", "WOOT", "woot"]
fiveB = foldr max 'a' "fear is the little death"
fiveC = foldr (&&) True [False, True]
fiveD = foldr (||) False [False, True]
fiveE = foldl (\x y -> x ++ (show y)) "" [1..5]
fiveF = foldr (\x y -> const (head . show $ x) y) 'a' [1..5]
fiveG = foldr const '0' "tacos"
fiveH = foldl (flip const) '0' "burritos"
fiveI = foldl (\x y -> const x (head . show $ y)) 'z' [1..5]
-- 5):1 ends here
