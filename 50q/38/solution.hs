menor :: String -> String -> Bool
menor _ "" = False
menor "" _ = True
menor (h:t) (h':t')
    | h < h' = True
    | h == h' = menor t t'
    | otherwise = False