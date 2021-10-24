isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf l l'@(_:t) = l == l' || isSuffixOf l t