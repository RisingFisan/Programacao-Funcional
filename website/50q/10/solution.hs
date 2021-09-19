replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x
    | n < 0 = []
    | otherwise = x : replicate (n - 1) x