-- Versão sem funções de ordem superior (mantém última ocorrência)
nub :: Eq a => [a] -> [a]
nub [] = []
nub (h:t) = if h `elem` t then nub t else h : nub t

-- Versão sem funções de ordem superior (mantém primeira ocorrência)
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' a = if l `elem` i then nub' i else nub' i ++ [l]
    where i = init a
          l = last a

-- Versão com funções de ordem superior
nub'' :: Eq a => [a] -> [a]
nub'' [] = []
nub'' (h:t) = h : filter (/= h) (nub'' t)