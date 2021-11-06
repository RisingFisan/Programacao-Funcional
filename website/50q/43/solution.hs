constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (l:ls) = insereMSet l (constroiMSet ls)
-- Função `insereMSet` definida na questão 41

-- em alternativa, esta solução não cria a lista "ao contrário", apesar de estar correto das duas formas:
constroiMSet' :: Ord a => [a] -> [(a,Int)]
constroiMSet' [] = []
constroiMSet' l = insereMSet (last l) (constroiMSet' (init l))