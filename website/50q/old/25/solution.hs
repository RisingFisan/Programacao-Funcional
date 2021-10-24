insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (h:t)
    | x > h = h : insert x t
    | otherwise = x : h : t