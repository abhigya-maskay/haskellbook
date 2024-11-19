z = 7

x = y ^ 2

waxOn = x * 5

y = z + 8

-- waxOn is 1125
-- 1.
a1 = 10 + waxOn -- This will return 1135

a2 = (+ 10) waxOn -- Also 1135

a3 = (-) 15 waxOn -- This should be -1110

a4 = (-) waxOn 15 -- This should be 1110

triple x = x * 3

-- 3.
-- This should return waxOn * 3 which will be 3375
c1 = triple waxOn

-- 4.
waxOn' = x * 5
  where
    x = y ^ 2
    y = z + 8
    z = 7

-- 6.
waxOff x = triple x

-- 7.
g1 = waxOff 10 -- 30
g2 = waxOff (-50) -- -150
