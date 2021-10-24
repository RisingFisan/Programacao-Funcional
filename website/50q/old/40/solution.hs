removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] = []
removeMSet x ((a,n):xs) = if x == a then xs else (a,n) : removeMSet x xs