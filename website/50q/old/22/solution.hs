remove :: Eq a => [a] -> [a] -> [a]
remove l [] = l
remove [] _ = []
remove l (h:t) = remove (delete h l) t
-- Função `delete` definida na questão anterior