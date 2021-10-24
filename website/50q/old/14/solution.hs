inits :: [a] -> [[a]]
inits [] = [[]]
inits l = inits (init l) ++ [l]