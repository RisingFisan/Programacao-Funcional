tails :: [a] -> [[a]]
tails [] = [[]]
tails l = l : tails (tail l)