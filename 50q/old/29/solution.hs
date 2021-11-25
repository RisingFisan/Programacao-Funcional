temRepetidos ::  Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (h:t) = h `elem` t || temRepetidos t