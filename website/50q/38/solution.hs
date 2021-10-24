menor :: String -> String -> Bool
menor _ "" = False
menor "" _ = True
menor (h:t) (h':t') = h < h' || menor t t'