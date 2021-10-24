pMaior :: Ord a => [a] -> Int
pMaior [_] = 0
pMaior (h:t)
    | h >= (t !! x) = 0
    | otherwise = 1 + x
    where x = pMaior t