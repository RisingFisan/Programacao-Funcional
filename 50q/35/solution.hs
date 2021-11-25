lookup :: Eq a => a -> [(a,b)] -> Maybe b
lookup _ [] = Nothing
lookup e ((a,b):t)
    | e == a = Just b
    | otherwise = lookup e t