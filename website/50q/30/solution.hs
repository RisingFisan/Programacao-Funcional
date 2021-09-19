algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (h:t)
    | h `elem` ['0'..'9'] = h : algarismos t
    | otherwise = algarismos t