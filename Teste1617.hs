import Data.Maybe (fromMaybe)

-- Exercicio 1

type MSet a = [(a,Int)]

ex1 :: MSet Char
ex1 = [('b',4),('a',2),('c',1)]

cardMSet ::  MSet a -> Int
cardMSet = foldr ((+) . snd) 0

moda :: MSet a -> [a]
moda = fst . foldr (\(x,n) (acc,accn) -> if n >= accn then (if n > accn then [x] else x:acc,n) else (acc,accn)) ([],0)

converteMSet :: MSet a -> [a]
converteMSet = foldr (\(x,n) -> (++) $ replicate n x) []

addNcopies :: Eq a => MSet a -> a -> Int -> MSet a
addNcopies mset elm num = foldr (\(x,n) -> (:) (x,n + (if x == elm then num else 0))) [] mset

-- Exercicio 2

data SReais = AA Double Double | FF Double Double
            | AF Double Double | FA Double Double
            | Uniao SReais SReais

ex2 = Uniao (Uniao (AA 4.2 5.5) (AF 3.1 7.0)) (FF (-12.3) 30.0)

instance Show SReais where
    show (Uniao a b) = "(" ++ show a ++ " U " ++ show b ++ ")"
    show intv = ch1 ++ num1 ++ "," ++ num2 ++ ch2
        where (ch1,num1,num2,ch2) = case intv of AA a b -> ("]",show a,show b,"[")
                                                 AF a b -> ("]",show a,show b,"]")
                                                 FA a b -> ("[",show a,show b,"[")
                                                 FF a b -> ("[",show a,show b,"]")

pertence :: Double -> SReais -> Bool
pertence num (Uniao a b) = pertence num a || pertence num b
pertence num intv = (af1 && num >= num1 || num > num1) && (af2 && num <= num2 || num < num2)
    where (af1,num1,num2,af2) = case intv of AA a b -> (False,a,b,False)
                                             AF a b -> (False,a,b,True)
                                             FA a b -> (True,a,b,False)
                                             FF a b -> (True,a,b,True)

tira ::  Double -> SReais -> SReais
tira n (Uniao a b) | pertence n a = Uniao (tira n a) b
                   | pertence n b = Uniao a (tira n b)
                   | otherwise = (Uniao a b)
tira n intv = case intv of AA a b -> if n > a && n < b then (Uniao (AA a n) (AA n b)) else intv
                           AF a b -> if n == b then AA a b else if n > a && n < b then (Uniao (AA a n) (AF n b)) else intv
                           FA a b -> if n == a then AA a b else if n > a && n < b then (Uniao (FA a n) (AA n b)) else intv
                           FF a b -> if n == b then FA a b else if n == a then AF a b else if n > a && n < b then (Uniao (FA a n) (AF n b)) else intv

-- Exercicio 3

data RTree a = R a [RTree a]

percorre :: [Int] -> RTree a -> Maybe [a]
percorre [] (R a _) = Just [a]
percorre _ (R a []) = Nothing
percorre (h:t) (R a r) | length r < h || null auxX = Nothing
                       | otherwise = Just (a:auxX)
    where aux = percorre t (r !! (h - 1))
          auxX = fromMaybe [] aux

procura :: Eq a => a -> RTree a -> Maybe [Int]
procura n (R a r) | n == a = Just []
                  | null r = Nothing
                  | otherwise = foldl (\acc num -> if procura n (r !! (num - 1)) == Nothing then acc else Just (num:fromMaybe [] (procura n (r !! (num - 1))))) Nothing [1..length r]