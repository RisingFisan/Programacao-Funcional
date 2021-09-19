elemMSet ::  Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet a ((x,_):xs) = a == x || elemMSet a xs