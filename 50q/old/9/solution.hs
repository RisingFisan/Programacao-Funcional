elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (h:t) = x == h || elem x t