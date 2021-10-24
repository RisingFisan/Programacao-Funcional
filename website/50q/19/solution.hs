idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ [] = []
idade ano idadeX ((nome,nasc):t)
    | ano - nasc >= idadeX = nome : idade ano idadeX t
    | otherwise = idade ano idadeX t