isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (h:s:t) = s >= h && isSorted (s : t)
