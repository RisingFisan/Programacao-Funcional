concat :: [[a]] -> [a]
concat [] = []
concat (h:t) = h ++ concat t