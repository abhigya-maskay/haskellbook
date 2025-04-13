-- 1)
id :: a -> a
id a = a

-- 2)
f :: a -> a -> a
f x y = x

f' :: a -> a -> a
f' x y = y

-- 3)
g :: a -> b -> b
g x y = y

-- The behavior of the function does not change when the types of a and b change
