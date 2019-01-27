-- Exercicio 1

isSorted :: (Ord a) => [a] -> Bool
isSorted (a:b:t) = a <= b && isSorted (b:t)
isSorted _ = True

inits :: [a] -> [[a]]
inits [] = [[]]
inits l = inits (init l) ++ [l]

-- Exercicio 2

maximumMB :: (Ord a) => [Maybe a] -> Maybe a
maximumMB = foldl (\acc x -> if x `maiorQue` acc then x else acc) Nothing
    where maiorQue (Just a) (Just b) = a > b
          maiorQue _ Nothing = True
          maiorQue Nothing _ = False

-- Exercicio 3

data LTree a = Tip a | Fork (LTree a) (LTree a)

listaLT :: LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork a b) = listaLT a ++ listaLT b

instance (Show a) => Show (LTree a) where
    show (Tip a) = show a ++ "\n"
    show (Fork a b) = mostra 1 a ++ mostra 1 b

mostra :: (Show a) => Int -> LTree a -> String
mostra n (Tip a) = replicate n '.' ++ show a ++ "\n"
mostra n (Fork a b) = mostra (n + 1) a ++ mostra (n + 1) b

-- Exercicio 4

maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = foldl (\acc x -> max (sum x) acc) (sum l) (inits l)

-- Exercicio 5

type RelP a = [(a,a)]
type RelL a = [(a,[a])]
type RelF a = ([a], a -> [a])

convLP :: RelL a -> RelP a
convLP l = concat (map junta l)
    where junta (x,xs) = map (\y -> (x,y)) xs

convPL :: (Eq a) => RelP a -> RelL a
convPL [(x,y)] = [(x,[y])]
convPL (h:t) = junta h (convPL t)
    where junta (a,b) l = if a `elem` map (fst) l 
                          then map (\(c,d) -> if c == a then (c,b:d) else (c,d)) l 
                          else (a,[b]):l

criaRelPint :: Int -> IO (RelP Int)
criaRelPint 0 = return []
criaRelPint n = do
    putStr "Introduz dois numeros (separados por um espaco): "
    linha <- getLine
    let (num1,num2) = span (/= ' ') linha
    resto <- criaRelPint (n - 1)
    return $ (read num1,read num2) : resto

convFP :: (Eq a) => RelF a -> RelP a
convFP (l,f) = convLP $ map (\x -> (x,f x)) l

convPF :: (Eq a) => RelP a -> RelF a
convPF x = ((map fst y),f)
    where y = convPL x
          f a = foldl (\acc (b,c) -> if a == b then c else acc) [] y