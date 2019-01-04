-- Exercicio 1

any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (h:t) = f h || any' f t

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs
zipWith' _ _ _ = []

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (h:t) | f h = h : takeWhile' f t
                   | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (h:t) | f h = dropWhile' f t
                   | otherwise = t

span' :: (a-> Bool) -> [a] -> ([a],[a])
span' _ [] = ([],[])
span' f (h:t) | f h = (h:s1,s2)
              | otherwise = ([],h:t)
    where (s1,s2) = span' f t

deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy f x (h:t) | f x h = t
                   | otherwise = h : deleteBy f x t
                   
sortOn :: (Ord b) => (a -> b) -> [a] -> [a]
sortOn f [] = []
sortOn f (h:t) = insere (h) (sortOn f t)
    where insere x [] = [x]
          insere x (a:b) = if f x > f a then a:insere x b else x:a:b

-- Exercicio 2

type Polinomio = [Monomio]
type Monomio = (Float,Int)

selgrau :: Int -> Polinomio -> Polinomio
selgrau n ps = filter (\x -> snd x == n) ps  

conta :: Int -> Polinomio -> Int
conta n p = length $ filter (\x -> n == snd x) p

grau :: Polinomio -> Int
grau ps = foldl (\acc x -> if acc > snd x then acc else snd x) 0 ps

deriv :: Polinomio -> Polinomio
deriv ps = filter (/= (0,0)) $ map (\(b,e) -> if e > 0 then (b * fromIntegral e, e - 1) else (0,0)) ps

calcula :: Float -> Polinomio -> Float
calcula a = foldl (\acc (b,e) -> acc + b * (a ^ e)) 0

simp :: Polinomio -> Polinomio
simp = filter (\(b,e) -> e /= 0) 

mult :: Monomio -> Polinomio -> Polinomio
mult (x,y) = map (\(b,e) -> (b*x,y+e))

ordena :: Polinomio -> Polinomio
ordena = sortOn (snd)

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((b,e):ps) = (sum [bs | (bs,es) <- selgrau e ps] + b,e):normaliza [(bo,eo) | (bo,eo) <- ps, eo /= e]

soma :: Polinomio -> Polinomio -> Polinomio
soma p r = normaliza $ (++) p r

produto :: P
produto p1 p2 = foldl (\acc x -> soma (mult x p2) acc) [] p1
