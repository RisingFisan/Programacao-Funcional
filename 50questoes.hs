module FiftyQ where

import Data.List

-- 1

enumFromTo' :: Int -> Int -> [Int]
enumFromTo' start end 
    | start == end = [end]
    | otherwise = start:enumFromTo (start+1) end

-- 2

enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' start next end 
    | start > end = []
    | otherwise = start:enumFromThenTo' next (2 * next - start) end

-- 3

concat'' :: [a] -> [a] -> [a]
concat'' [] l = l
concat'' (h:t) l = h:concat'' t l

-- 4

getFromIndex :: [a] -> Int -> a
getFromIndex (h:t) n
    | n == 0 = h
    | otherwise = getFromIndex t (n - 1)

-- 5

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:t) = reverse' t ++ [h]

-- 6

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (h:t) = h : take' (n - 1) t

-- 7

drop' :: Int -> [a] -> [a]
drop' 0 l = l
drop' _ [] = []
drop' n (h:t) = drop' (n - 1) t

-- 8

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (h:t) (h':t') = (h,h'):zip' t t'

-- 9

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (h:t) = x == h || elem' x t

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

-- 13

concat' :: [[a]] -> [a]
concat' [] = []
concat' (h:t) = h ++ concat' t

-- 14

inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l] 

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

-- 20

nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (h:t) = h : filter (/= h) (nub' t)

-- OU

nub2 :: Eq a => [a] -> [a]
nub2 [] = []
nub2 (h:t) = if elem h t then nub2 t else h:nub2 t

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

insert ::  Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (l:ls) = if x <= l then x:l:ls else l:insert x ls

-- 26

unwords' :: [String] -> String
unwords' [] = ""
unwords' [x] = x
unwords' (word:words) = word ++ " " ++ unwords' words

-- 27

unlines' :: [String] -> String
unlines' [] = ""
unlines' (word:words) = word ++ "\n" ++ unlines' words

-- 28

pMaior :: Ord a => [a] -> Int
pMaior (h:t) = aux 0 0 h t
    where aux _ nm _ [] = nm
          aux n nm x (l:ls) = if l > x 
                              then aux (n + 1) (n + 1) l ls 
                              else aux (n + 1) nm x ls

-- 29

temRepetidos ::  Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (l:ls) = if elem l ls then True else temRepetidos ls

-- 30

algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos l = filter (`elem` ['0'..'9']) l

algarismos' [] = []
algarismos' (h:t) | elem h ['0'..'9'] = h:algarismos' t
                  | otherwise = algarismos' t

-- 31

posImpares ::  [a] -> [a]
posImpares l = aux 0 l
    where aux _ [] = []
          aux n (l:ls) = if odd n then l:aux (n + 1) ls else aux (n + 1) ls

-- 32

posPares :: [a] -> [a]
posPares l = aux 0 l
    where aux _ [] = []
          aux n (l:ls) = if even n then l:aux (n + 1) ls else aux (n + 1) ls

-- 33

isSorted :: Ord a => [a] -> Bool
isSorted [x] = True
isSorted (x:y:xs) = y >= x && isSorted (y:xs)

-- 34

iSort :: Ord a => [a] -> [a] -- QuickSort
iSort [] = []
iSort (l:ls) = iSort (maisPequenos ls) ++ [l] ++ iSort (maiores ls)
    where maisPequenos [] = []
          maisPequenos (x:xs) = if x <= l then x:maisPequenos xs else maisPequenos xs 
          maiores [] = []
          maiores (x:xs) = if x > l then x:maiores xs else maiores xs

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (l:ls) = maisPequenos ++ [l] ++ maiores
    where maisPequenos = quickSort $ filter (<=l) ls
          maiores = quickSort $ filter (>l) ls

-- 35

menor :: String -> String -> Bool
menor a b = a < b

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
constroiMSet (l:ls) = (l,1 + length (takeWhile (==l) ls)):constroiMSet (dropWhile (==l) ls)

-- 42

partitionEithers ::  [Either a b] -> ([a],[b])
partitionEithers l = (partitionLefts l, partitionRights l)
    where partitionLefts [] = []
          partitionLefts ((Left x):ls) = x:partitionLefts ls
          partitionLefts ((Right x):ls) = partitionLefts ls
          partitionRights [] = []
          partitionRights ((Left x):ls) = partitionRights ls
          partitionRights ((Right x):ls) = x:partitionRights ls

myPE :: [Either a b] -> ([a],[b])
myPE [] = ([],[])
myPE ((Left a):t) = (a:as,bs)
    where (as,bs) = myPE t
myPE ((Right b):t) = (as,b:bs)
    where (as,bs) = myPE t  

-- 43

catMaybes :: [Maybe a] -> [a]
catMaybes (m:ms) = case m of (Nothing) -> catMaybes ms
                             (Just x) -> x:catMaybes ms
catMaybes [] = []

mycatMaybes :: [Maybe a] -> [a]
mycatMaybes [] = []
mycatMaybes ((Just a):ms) = a:mycatMaybes ms
mycatMaybes (Nothing:ms) = mycatMaybes ms

-- 44

data Movimento = Norte | Sul | Este | Oeste deriving Show --(Show, Eq)

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao p [] = p
posicao (x, y) (m:ms) = posicao (case m of Norte -> (x, y + 1)
                                           Sul -> (x, y - 1)
                                           Este -> (x + 1, y)
                                           Oeste -> (x - 1, y)) ms

-- 45

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xi,yi) (xf,yf)
    | xi < xf = Norte:caminho (xi + 1, yi) (xf, yf)
    | xi > xf = Sul:caminho (xi - 1, yi) (xf, yf)
    | yi < yf = Este:caminho (xi, yi + 1) (xf, yf)
    | yi > yf = Oeste:caminho (xi, yi - 1) (xf, yf)
    | otherwise = []

-- 46

vertical :: [Movimento] -> Bool
vertical [] = True
vertical (l:ls) = case l of Este -> False
                            Oeste -> False
                            otherwise -> vertical ls

{- APENAS FUNCIONA SE MOVIMENTO TIVER A PROPRIEDADE EQ
vertical' :: [Movimento] -> Bool
vertical' l = foldl (\acc x -> if (x == Este || x == Oeste) then False else acc) True l
-}

-- 47

data Posicao = Pos Int Int deriving Show

maisCentral ::  [Posicao] -> Posicao
maisCentral ps = foldl1 (\(Pos xacc yacc) (Pos x y) -> if (xacc^2 + yacc^2) > (x^2 + y^2) then (Pos x y) else (Pos xacc yacc)) ps

mymaisCentral :: [Posicao] -> Posicao
mymaisCentral [(Pos x y)] = (Pos x y)
mymaisCentral ((Pos x y):(Pos a b):ps) = if (x^2 + y^2) < (a^2 + b^2) then maisCentral ((Pos x y):ps) else maisCentral ((Pos a b):ps)

-- 48

vizinhos ::  Posicao -> [Posicao] -> [Posicao]
vizinhos (Pos x y) ps = filter (\(Pos a b) -> (abs (a - x) + abs (b - y) == 1)) ps 

myvizinhos :: Posicao -> [Posicao] -> [Posicao]
myvizinhos _ [] = []
myvizinhos (Pos x y) ((Pos xv yv):ps) = if abs (x - xv) == 1 && y == yv || abs (y - yv) == 1 && x == xv 
                                        then (Pos xv yv):myvizinhos (Pos x y) ps 
                                        else myvizinhos (Pos x y) ps

-- 49

mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [(Pos x y)] = True
mesmaOrdenada ((Pos x y):(Pos x2 y2):ps) = y == y2 && mesmaOrdenada ((Pos x2 y2):ps) 

-- 50

data Semaforo = Verde | Amarelo | Vermelho deriving Show

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK ss = length [s | s <- ss, case s of Vermelho -> False; otherwise -> True] < 2
