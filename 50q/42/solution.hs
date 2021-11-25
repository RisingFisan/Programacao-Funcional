removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] = []
removeMSet x ((a,n):xs) 
    | x == a = if n > 1 then (a, n-1) : xs else xs
    | otherwise = (a,n) : removeMSet x xs