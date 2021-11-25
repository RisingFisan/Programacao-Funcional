insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = [(x,1)]
insereMSet x ((a,n):xs) = if x == a then (a,n+1) : xs else (a,n) : insereMSet x xs