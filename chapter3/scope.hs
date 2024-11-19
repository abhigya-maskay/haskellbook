-- 1. Everything is in scope
x = 5
y = 7
z = x * y

-- 2. h is not in scope
f = 3
g = 6 * f + h

-- 3. d is not in scope in the second line
area d = pi * (r * r)
r = d / 2

-- 4. This one has everything in scope
area' d = pi * (r * r) where
  r = d / 2
