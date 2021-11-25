take :: Int -> [a] -> [a]
take _ [] = []
take n (h:t)
    | n <= 0 = []
    | otherwise = h : take (n - 1) t