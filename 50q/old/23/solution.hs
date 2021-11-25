union :: Eq a => [a] -> [a] -> [a]
union l [] = l
union l (h:t)
    | h `elem` l = union l t
    | otherwise = union (l ++ [h]) t