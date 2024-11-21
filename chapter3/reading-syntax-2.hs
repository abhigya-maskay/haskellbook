-- [6, 12, 18]
a = concat [[1 * 6], [2 * 6], [3 * 6]]

-- "rainbow"
b = "rain" ++ drop 2 "elbow"

-- 10
c = 10 * head [1, 2, 3]

-- "Jules"
d = (take 3 "Julie") ++ (tail "yes")

-- [2,3,5,6,8,9]
e =
  concat
    [ tail [1, 2, 3],
      tail [4, 5, 6],
      tail [7, 8, 9]
    ]
