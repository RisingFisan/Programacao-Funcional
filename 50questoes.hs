module CinquentaQuestoes where

-- * Funções recursivas

-- ** 1) EnumFromTo
-- | Cria uma lista com todos os valores inteiros entre o mínimo e o máximo dados.

enumFromTo' :: Int -- ^ O valor mínimo da lista
            -> Int -- ^ O valor máximo da lista
            -> [Int] -- ^ A lista resultante
enumFromTo' min max 
    | min < max = min:enumFromTo' (min + 1) max
    | min == max = [max]

-- myEnumFromTo min max = reverse $ foldl (flip (:)) [] [min..max]
-- myEnumFromTo' min max = foldl (\acc x -> acc ++ [x]) [] [min..max]

-- ** 2) EnumFromThenTo
-- | Cria uma lista com os valores inteiros entre o mínimo e o máximo dados, com "passos" iguais à diferença entre o mínimo e o segundo valor dado.
--  
-- __Exemplo:__
--  
-- >>> enumFromThenTo' 2 4 10
-- [2,4,6,8,10]
enumFromThenTo' :: Int -- ^ O valor mínimo da lista 
                -> Int -- ^ O segundo valor da lista
                -> Int -- ^ O valor máximo da lista
                -> [Int] -- ^ A lista resultante
enumFromThenTo' min sec max 
    | min < max = min:enumFromThenTo' sec (sec + abs (min - sec)) max
    | min == max = [max]
    | min > max = []

-- ** 3) Concatenar (+++)
-- | Dadas duas listas, junta-as numa só, pela ordem em que foram fornecidas e sem remover elementos de nenhuma.
--
-- __Exemplo:__
--
-- >>> [1,2,3] +++ [4,5,6]
-- [1,2,3,4,5,6]

(+++) :: [a] -- ^ Primeira lista 
      -> [a] -- ^ Segunda lista
      -> [a] -- ^ Lista resultante da união das duas listas anteriores
(+++) [] l = l
(+++) (a:as) b = a:(+++) as b

pp l1 l2 = foldr (:) l2 l1

pp' :: [a] -> [a] -> [a]
pp' = flip $ foldr (:)

-- ** 4) Encontrar Na Lista (!!!)
-- | Encontra o elemento de uma dada lista a partir do índice desse elemento.
--
-- __Exemplo:__
--
-- >>> [1,2,3,4,5,6] !!! 3
-- 4

(!!!) :: [a] -- ^ Lista cujo elemento se pretende encontrar
      -> Int -- ^ Índice do dito elemento
      -> a -- ^ Elemento da lista com o índice fornecido
(!!!) (l:ls) 0 = l
(!!!) (l:ls) n = (!!!) ls (n - 1)

(!!!!) l n = head (foldl (\acc _ -> tail acc) l [1..n])

-- ** 5) Reverse
-- | Inverte a ordem dos elementos de uma lista dada.
--
-- __Exemplo:__
--
-- >>> reverse [1,2,3,4,5]
-- [5,4,3,2,1]

reverse' :: [a] -- ^ Lista original
         -> [a] -- ^ lista revertida
reverse' [x] = [x]
reverse' (l:ls) = reverse' ls ++ [l]

myreverse :: [a] -> [a]
myreverse = foldl (flip (:)) []

-- ** 6) Take
-- | Devolve uma lista com os primeiros n elementos da lista fornecida.
--
-- __Exemplo:__
--
-- >>> take' 4 "abcdefgh"
-- "abcd"

take' :: Int -- ^ nº de elementos a retirar da lista
      -> [a] -- ^ Lista cujos elementos vão ser retirados
      -> [a] -- ^ Lista com os elementos retirados da primeira lista
take' 0 _ = []
take' n [] = []
take' n (l:ls) = l:take' (n - 1) ls

mytake n = foldl (\acc x -> if length acc < n then acc ++ [x] else acc) [] 

-- 7

drop' ::  Int -> [a] -> [a]
drop' 0 l = l
drop' n [] = []
drop' n (l:ls) = drop' (n-1) ls

mydrop n l = foldr (\x acc -> if length acc < length l - n then x:acc else acc) [] l

-- 8

zip' ::  [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (la:las) (lb:lbs) = (la,lb):zip' las lbs 

-- 9

elem' ::  Eq a => a -> [a] -> Bool
elem' _ [] = False
-- elem' x [a] = x == a
elem' x (l:ls) = x == l || elem' x ls

myelem :: Eq a => a -> [a] -> Bool
myelem a = foldr ((||) . (==) a) False

-- 10

replicate' ::  Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x:replicate' (n - 1) x

myreplicate n x = foldr (\_ -> (:) x) [] [1..n]

-- 11

intersperse' ::  a -> [a] -> [a]
intersperse' _ [x] = [x]
intersperse' x (l:ls) = l:x:intersperse' x ls

myintersperse :: a -> [a] -> [a]
myintersperse x = foldl (\acc y -> if length acc == 0 then [y] else acc ++ [x,y]) []

-- 12

group' :: Eq a => [a] -> [[a]]
group' [] = []
group' (x:l) = (x:equalToX):group' restOfList
    where (equalToX,restOfList) = myspan (igualA x) l
          myspan :: (a -> Bool) -> [a] -> ([a],[a])
          myspan func l = (l1 func l,l2 func l)
          l1 _ [] = []
          l1 func (p:ps) = if func p then p:l1 func ps else []
          l2 _ [] = []
          l2 func (p:ps) = if func p then l2 func ps else (p:ps)
          igualA a b = a == b

mygroup :: Eq a => [a] -> [[a]]
mygroup [] = []
mygroup (x:xs) = (x:aux xs):aux2 xs
    where aux (y:ys) = if y == x then y:aux ys else []
          aux [] = []
          aux2 [] = []
          aux2 (z:zs) = if z == x then aux2 zs else mygroup (z:zs)

group'' :: Eq a => [a] -> [[a]]
group'' [] = []
group'' l@(x:_) = equalToX : group'' restOfList
    where (equalToX,restOfList) = span (== x) l 

-- 13

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- 14

inits :: [a] -> [[a]]
inits [x] = [[],[x]]
inits l = inits (init l) ++ [l]

inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = myjoin (inits (myinit l)) [l]
    where myinit [l] = []
          myinit (l:ls) = l:myinit ls
          myjoin [] b = b
          myjoin (a:as) b = a:myjoin as b

myinits :: [a] -> [[a]]
myinits [x] = [[],[x]]
myinits (x:xs) = [] : aux x (myinits xs)
    where aux a (h:t) = (a:h):aux a t
          aux a [] = []

-- 15

tails :: [a] -> [[a]]
tails [x] = [[x],[]]
tails l = l:tails (tail l)

mytails :: [a] -> [[a]]
mytails [] = [[]]
mytails l = l:tails (mytail l)
    where mytail (l:ls) = ls

-- 16

isPrefixOf ::  Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (l1:l1s) (l2:l2s) = if l1 == l2 then isPrefixOf l1s l2s else False

-- 17

isSuffixOf ::  Eq a => [a] -> [a] -> Bool
isSuffixOf [] _ = True
-- isSuffixOf [x] l = x == last l
isSuffixOf _ [] = False
isSuffixOf (x:xs) (y:ys) = if x == y then isSuffixOf xs ys else isSuffixOf (x:xs) ys

-- 18

isSubsequenceOf ::  Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (y:ys) = if x == y then isSubsequenceOf xs ys else isSubsequenceOf (x:xs) ys

-- 19

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices x l = [n | n <- [0..(length l - 1)], x == (l !! n)]
-- OU:
elemIndices' x l = filter (\n -> x == (l !! n)) [0..(length l - 1)]
-- OU:
elemIndices'' x l = foldl (\acc n -> if x == (l !! n) then acc ++ [n] else acc) [] [0..(length l - 1)]
-- OU:
elemIndices2 _ [] = []
elemIndices2 x l = elemIndComN 0 x l
    where elemIndComN _ _ [] = []
          elemIndComN n x (l:ls) = if x == l then n:elemIndComN (n+1) x ls else elemIndComN (n+1) x ls

-- 20

nub ::  Eq a => [a] -> [a]
nub [] = []
nub (x:l) = if elem x l then nub l else x:nub l

-- 21

delete ::  Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (l:ls) 
    | x == l = ls
    | elem x ls = l:delete x ls
    | otherwise = (l:ls)

mydelete :: Eq a => a -> [a] -> [a]
mydelete _ [] = []
mydelete x (y:ys) = if x == y then ys else y : mydelete x ys

-- 22

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) l [] = l
(\\) [] _ = []
(\\) l (y:ys) = (\\) (delete y l) ys

-- 23

union ::  Eq a => [a] -> [a] -> [a]
union [] l = l
union l [] = l
union l (y:ys) = if elem y l then union l ys else union (l ++ [y]) ys

-- 24

intersect ::  Eq a => [a] -> [a] -> [a]
intersect _ [] = []
intersect [] _ = []
intersect (x:xs) l = if elem x l then x:intersect xs l else intersect xs l

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
