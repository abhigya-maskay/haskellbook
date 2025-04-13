-- 1) Breaks
bigNum = (^) 5 $ 10
wahoo = bigNum * 10 -- Was broken because bigNum is not a function

-- 2) Works Fine
x = print
y = print "woohoo!"
z = x "hello world"

-- 3) Breaks
a = (+)
b = 5
c = b `a` 10
d = c `a` 200 -- Was originally broken because b and c are not functions

-- 4) Breaks
a' = 12 + b'
b' = 10000 * c'
c' = 10 -- Was initially broken because c' was not defined
