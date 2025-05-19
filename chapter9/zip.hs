myZip :: [a] -> [b] -> [(a,b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y) : (myZip xs ys)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith zipper (x:xs) (y:ys) = (zipper x y):(myZipWith zipper xs ys)

myZip' :: [a] -> [b] -> [(a,b)]
myZip' = myZipWith (,)
