delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (h:t)
    | x == h = t
    | otherwise = h : delete x t