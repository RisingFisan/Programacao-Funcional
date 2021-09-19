drop :: Int -> [a] -> [a]
drop _ [] = []
drop n (h:t)
    | n <= 0 = h : t
    | otherwise = drop (n - 1) t