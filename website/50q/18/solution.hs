cola :: [(String,b,c)] -> String
cola [] = ""
cola ((a,b,c):t) = a ++ cola t