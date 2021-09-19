-- Versão sem funções de ordem superior
group :: Eq a => [a] -> [[a]]
group [] = []
group [x] = [[x]]
group (h:t)
    | h == head hr = (h : hr) : tr 
    | otherwise = [h] : hr : tr
    where (hr:tr) = group t

-- Versão com funções de ordem superior
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' (h:t) = (h:takeWhile (== h) t) : group' (dropWhile (== h) t)