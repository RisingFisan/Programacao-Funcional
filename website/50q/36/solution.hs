preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [x] = [x]
preCrescente (h:s:t)
    | s >= h = h : preCrescente (s:t)
    | otherwise = [h]