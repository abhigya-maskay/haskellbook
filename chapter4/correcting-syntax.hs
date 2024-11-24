-- 1.
x = (+)

f xs = w `x` 1
  where
    w = length xs

--2.
id = \x -> x

--3.
fst' (a,b) = a
