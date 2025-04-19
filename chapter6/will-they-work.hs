-- 1) will work
a = max (length [1,2,3]) (length [8,9,10,11,12])

-- 2) will work
b = compare (3*4) (3*5)

-- 3) will not work because the two arguments are not of the same type
c = compare "Julie" True

-- 4) Will work
d = (5 + 3) > (3 + 6)
