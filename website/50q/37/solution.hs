lengthMSet ::  [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((x,n):xs) = n + lengthMSet xs
