intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect (h:t) l
    | h `elem` l = h : intersect t l
    | otherwise = intersect t l