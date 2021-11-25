-- Versão sem funções de ordem superior
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices x l = [n | n <- [0..(length l - 1)], x == (l !! n)]

-- Versão com funções de ordem superior
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' x (h:t)
    | x == h = 0 : map (+1) (elemIndices' x t)
    | otherwise = map (+1) (elemIndices' x t)