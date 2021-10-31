preCrescente :: Ord a => [a] -> [a]
preCrescente l = preCrescenteAux l [] []

preCrescenteAux :: Ord a => [a] -> [a] -> [a] -> [a]
preCrescenteAux [] _ l = l
preCrescenteAux (h:t) curMax maxL
    | not (null t) && head t >= h = preCrescenteAux t newMax maxL
    | otherwise = preCrescenteAux t [] (if length newMax > length maxL then newMax else maxL)
    where newMax = curMax ++ [h]