zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (h:t) (h':t') = (h,h') : zip t t'