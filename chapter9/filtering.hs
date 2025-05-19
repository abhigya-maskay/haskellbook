-- [[file:filtering.org::One][One]]
mult3 :: Integral a => [a] -> [a]
mult3 = filter (\x -> x `mod` 3 == 0)
one = mult3 [1..30]
-- One ends here

-- [[file:filtering.org::Two][Two]]
mult3Length :: Integral a => [a] -> Int
mult3Length = length . mult3
two = mult3Length [1..30]
-- Two ends here

-- [[file:filtering.org::Three][Three]]
filterArticlesList :: [String] -> [String]
filterArticlesList = filter (\x -> not . elem x $ ["a", "an", "the"])

filterArticles :: String -> [String]
filterArticles = filterArticlesList . words
-- Three ends here
