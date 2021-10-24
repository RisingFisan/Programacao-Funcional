iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert h (iSort t)

-- A este tipo de ordenação chamamos "insertion sort".

-- Outras formas de ordenar listas:

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (l:ls) = maisPequenos ++ [l] ++ maiores
    where maisPequenos = quickSort $ filter (<=l) ls
          maiores = quickSort $ filter (>l) ls

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort l = merge (mergeSort metade1) (mergeSort metade2)
    where (metade1,metade2) = splitAt (div (length l) 2) l
          merge :: Ord a => [a] -> [a] -> [a]
          merge [] l = l
          merge l [] = l
          merge (a:b) (c:d) = if a < c then a:merge b (c:d) else c:merge (a:b) d