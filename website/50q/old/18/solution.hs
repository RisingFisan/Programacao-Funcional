isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (h:t) (h':t') = h == h' && isSubsequenceOf t t' || isSubsequenceOf (h:t) t'