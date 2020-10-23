module FiftyQ where

import Data.List

-- 1

enumFromTo' :: Int -> Int -> [Int]
enumFromTo' start end 
    | start > end = []
    | otherwise = start:enumFromTo (start+1) end

enumFromToScary :: Int -> Int -> [Int]
enumFromToScary = curry (uncurry (flip takeWhile) . split (iterate (+1) . fst) ((>=) . snd))
    where split f g x = (f x, g x)

-- 2

enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' start next end
    | start > end && next - start > 0 || start < end && next - start < 0 = []
    | otherwise = start:enumFromThenTo' next (2 * next - start) end

-- 3

concat'' :: [a] -> [a] -> [a]
concat'' [] l = l
concat'' (h:t) l = h:concat'' t l

concat_mini :: [a] -> [a] -> [a]
concat_mini = flip $ foldr (:)

-- 4

getFromIndex :: [a] -> Int -> a
getFromIndex (h:t) n
    | n == 0 = h
    | otherwise = getFromIndex t (n - 1)

getFromIndex' :: [a] -> Int -> a
getFromIndex' l n = head (foldl (curry $ tail . fst) l [1..n])

-- 5

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:t) = reverse' t ++ [h]

reverse_mini :: [a] -> [a]
reverse_mini = foldl (flip (:)) []

-- 6

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (h:t) = h : take' (n - 1) t

take_fold n = foldl (\acc x -> if length acc < n then acc ++ [x] else acc) [] 

-- 7

drop' :: Int -> [a] -> [a]
drop' 0 l = l
drop' _ [] = []
drop' n (_:t) = drop' (n - 1) t

drop_fold :: Int -> [a] -> [a]
drop_fold n l = foldr (\x acc -> if length acc < length l - n then x:acc else acc) [] l

-- 8

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (h:t) (h':t') = (h,h'):zip' t t'

-- 9

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (h:t) = x == h || elem' x t

elem_mini :: Eq a => a -> [a] -> Bool
elem_mini a = foldr ((||) . (==) a) False

-- 10

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x:replicate' (n - 1) x

-- 11

intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [h] = [h]
intersperse' x (h:t) = h:x:intersperse' x t 

-- 12

group' :: Eq a => [a] -> [[a]]
group' [] = []
group' (h:t) = (h:takeWhile (== h) t) : group' (dropWhile (== h) t)

group'' :: Eq a => [a] -> [[a]]
group'' [] = []
group'' (x:xs) = (x:aux xs):aux2 xs
    where aux (y:ys) = if y == x then y:aux ys else []
          aux [] = []
          aux2 [] = []
          aux2 (z:zs) = if z == x then aux2 zs else group'' (z:zs)

-- 13

concat' :: [[a]] -> [a]
concat' [] = []
concat' (h:t) = h ++ concat' t

concat'_mini :: [[a]] -> [a]
concat'_mini = foldr (++) []

-- 14

inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l] 

inits'' :: [a] -> [[a]]
inits'' [x] = [[],[x]]
inits'' (x:xs) = [] : aux x (inits'' xs)
    where aux a (h:t) = (a:h):aux a t
          aux _ [] = []

-- 15

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l = l : tails' (tail l)

-- 16

isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (h:t) (h':t') = h == h' && isPrefixOf' t t'

-- 17

isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] _ = True
isSuffixOf' _ [] = False
isSuffixOf' l l'@(_:t) = l == l' || isSuffixOf' l t

-- 18

isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' (h:t) (h':t') = h == h' && isSubsequenceOf' t t' || isSubsequenceOf' (h:t) t'

-- 19

elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' x (h:t)
    | x == h = 0 : map (+1) (elemIndices' x t)
    | otherwise = map (+1) (elemIndices' x t)

elemIndices'' :: Eq a => a -> [a] -> [Int]
elemIndices'' x l = [n | n <- [0..(length l - 1)], x == (l !! n)]

-- 20

nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (h:t) = h : filter (/= h) (nub' t)

nub2 :: Eq a => [a] -> [a]
nub2 [] = []
nub2 (h:t) = if h `elem` t then nub2 t else h:nub2 t

-- 21

delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' x (h:t)
    | x == h = t
    | otherwise = h:delete' x t

-- 22

remove :: Eq a => [a] -> [a] -> [a]
remove l [] = l
remove [] _ = []
remove l (h:t) = remove (delete' h l) t

-- 23

union' :: Eq a => [a] -> [a] -> [a]
union' l [] = l
union' l (h:t)
    | h `elem` l = union' l t
    | otherwise = union' (l ++ [h]) t

-- 24

intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] _ = []
intersect' (h:t) l
    | h `elem` l = h:intersect' t l
    | otherwise = intersect' t l

-- 25

insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (h:t)
    | x > h = h:insert' x t
    | otherwise = x:h:t

-- 26

unwords' :: [String] -> String
unwords' [] = ""
unwords' (h:t) = h ++ (if null t then "" else " ") ++ unwords' t

-- 27

unlines' :: [String] -> String
unlines' [] = ""
unlines' (h:t) = h ++ "\n" ++ unlines' t

-- 28

pMaior :: Ord a => [a] -> Int
pMaior [_] = 0 -- [_] é uma lista com apenas um elemento, o underscore apenas substitui o nome de uma variável
pMaior (h:t)
    | h > (t !! x) = 0
    | otherwise = 1 + x
    where x = pMaior t

-- OU (a segunda versão usa uma função auxiliar, mas é mais intuitiva)

pMaior' :: Ord a => [a] -> Int
pMaior' (h:t) = aux 0 0 h t
    where aux _ nm _ [] = nm
          aux n nm x (l:ls) = if l > x -- n é o índice "atual", nm é o índice do valor máximo da lista
                              then aux (n + 1) (n + 1) l ls 
                              else aux (n + 1) nm x ls

-- 29

temRepetidos ::  Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (h:t) = h `elem` t || temRepetidos t

-- 30

algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (h:t)
    | h `elem` ['0'..'9'] = h:algarismos t
    | otherwise = algarismos t

algarismos' :: [Char] -> [Char]
algarismos' [] = []
algarismos' l = filter (`elem` ['0'..'9']) l

-- 31

posImpares ::  [a] -> [a]
posImpares [] = []
posImpares [_] = []
posImpares (h:s:t) = s:posImpares t

-- 32

posPares :: [a] -> [a]
posPares [] = []
posPares [x] = [x]
posPares (h:s:t) = h:posPares t

-- 33

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (h:s:t) = s >= h && isSorted (s:t)

-- 34

-- Função insert' definida na questão 25

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert h (iSort t)

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

-- 35

menor :: String -> String -> Bool
menor _ "" = False
menor "" _ = True
menor (h:t) (h':t') = h < h' || menor t t'


-- 36

elemMSet ::  Eq a => a -> [(a,Int)] -> Bool
elemMSet a [] = False
elemMSet a ((x,n):xs) = a == x || elemMSet a xs

-- 37

lengthMSet ::  [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((x,n):xs) = n + lengthMSet xs

-- 38

converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((x,1):xs) = x:converteMSet xs
converteMSet ((x,n):xs) = x:converteMSet ((x,n-1):xs)

-- OU

converteMSet' :: [(a,Int)] -> [a]
converteMSet' [] = []
converteMSet' ((x,n):xs) = replicate n x ++ converteMSet xs

-- 39

insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = [(x,1)]
insereMSet x ((a,n):xs) = if x == a then (a,n+1):xs else (a,n):insereMSet x xs

-- 40

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] = []
removeMSet x ((a,n):xs) = if x == a then xs else (a,n):removeMSet x xs

-- 41

constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (l:ls) = insereMSet l (constroiMSet ls)

-- OU

constroiMSet' :: Ord a => [a] -> [(a,Int)]
constroiMSet' [] = []
constroiMSet' (l:ls) = (l,1 + length (filter (==l) ls)):constroiMSet' (filter (/=l) ls)

-- 42

partitionEithers ::  [Either a b] -> ([a],[b])
partitionEithers l = (partitionLefts l, partitionRights l)
    where partitionLefts [] = []
          partitionLefts ((Left x):ls) = x:partitionLefts ls
          partitionLefts ((Right _):ls) = partitionLefts ls
          partitionRights [] = []
          partitionRights ((Left _):ls) = partitionRights ls
          partitionRights ((Right x):ls) = x:partitionRights ls

partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' [] = ([],[])
partitionEithers' ((Left a):t) = (a:as,bs)
    where (as,bs) = partitionEithers' t
partitionEithers' ((Right b):t) = (as,b:bs)
    where (as,bs) = partitionEithers' t  

-- 43

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (m:ms) = case m of Nothing -> catMaybes ms
                             Just x -> x:catMaybes ms

catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' ((Just a):ms) = a:catMaybes' ms
catMaybes' (Nothing:ms) = catMaybes' ms

-- 44

data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao p [] = p
posicao (x, y) (m:ms) = posicao (case m of Norte -> (x, y + 1)
                                           Sul -> (x, y - 1)
                                           Este -> (x + 1, y)
                                           Oeste -> (x - 1, y)) ms

-- 45

caminho :: (Int, Int) -> (Int, Int) -> [Movimento]
caminho (xi, yi) (xf, yf) | xi < xf   = Este : caminho (xi + 1, yi) (xf, yf)
                          | xi > xf   = Oeste : caminho (xi - 1, yi) (xf, yf)
                          | yi < yf   = Norte : caminho (xi, yi + 1) (xf, yf)
                          | yi > yf   = Sul : caminho (xi, yi - 1) (xf, yf)
                          | otherwise = []

-- 46

vertical :: [Movimento] -> Bool
vertical [] = True
vertical (l:ls) = case l of Este -> False
                            Oeste -> False
                            _ -> vertical ls

-- 47

data Posicao = Pos Int Int deriving Show

maisCentral ::  [Posicao] -> Posicao
maisCentral = foldl1 (\(Pos xacc yacc) (Pos x y) -> if (xacc^2 + yacc^2) > (x^2 + y^2) then Pos x y else Pos xacc yacc)

maisCentral' :: [Posicao] -> Posicao
maisCentral' [Pos x y] = Pos x y
maisCentral' ((Pos x y):(Pos a b):ps) = if (x^2 + y^2) < (a^2 + b^2) then maisCentral (Pos x y : ps) else maisCentral (Pos a b : ps)

-- 48

vizinhos ::  Posicao -> [Posicao] -> [Posicao]
vizinhos (Pos x y) = filter (\(Pos a b) -> abs (a - x) + abs (b - y) == 1)

vizinhos' :: Posicao -> [Posicao] -> [Posicao]
vizinhos' _ [] = []
vizinhos' (Pos x y) ((Pos xv yv):ps) = if abs (x - xv) == 1 && y == yv || abs (y - yv) == 1 && x == xv 
                                       then Pos xv yv : vizinhos' (Pos x y) ps 
                                       else vizinhos' (Pos x y) ps

-- 49

mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [Pos x y] = True
mesmaOrdenada ((Pos x y):(Pos x2 y2):ps) = y == y2 && mesmaOrdenada (Pos x2 y2 : ps)

-- 50

data Semaforo = Verde | Amarelo | Vermelho deriving Show

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK ss = length [s | s <- ss, case s of Vermelho -> False; _ -> True] < 2
