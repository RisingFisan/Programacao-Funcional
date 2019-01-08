import Data.Char (isDigit,isAlpha)
import System.Random

-- Exercicio 1

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices x l = foldl (\acc n -> if l !! n == x then acc ++ [n] else acc) [] [0..(length l - 1)]

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (a:b) (c:d) | a == c = isSubsequenceOf b d
                            | otherwise = isSubsequenceOf (a:b) d

-- Exercicio 2

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

lookupAP :: Ord a => a -> BTree (a,b) -> Maybe b
lookupAP _ Empty = Nothing
lookupAP x (Node (a,b) l r) | x == a = Just b
                            | x < a = lookupAP x l
                            | otherwise = lookupAP x r

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT _ Empty _ = Empty
zipWithBT _ _ Empty = Empty
zipWithBT f (Node a esq dir) (Node b left right) = Node (f a b) (zipWithBT f esq left) (zipWithBT f dir right)

-- Exercicio 3

digitAlpha :: String -> (String,String)
digitAlpha = foldr (\x (ds,as) -> if isDigit x then (x:ds,as) else if isAlpha x then (ds,x:as) else (ds,as)) ([],[]) 

-- Exercicio 4

data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)

firstSeq :: Seq a -> a
firstSeq (Cons a s) = a
firstSeq (App Nil s) = firstSeq s
firstSeq (App s _) = firstSeq s

dropSeq :: Int -> Seq a -> Seq a
dropSeq _ Nil = Nil
dropSeq n (Cons a s) = dropSeq (n - 1) s
dropSeq n (App s1 s2) | n > nx = dropSeq (n - nx) s2
                      | n == nx = s2
                      | otherwise = (App (dropSeq n s1) s2)
    where nx = contaCons s1

contaCons :: Seq a -> Int
contaCons Nil = 0
contaCons (Cons _ s) = 1 + contaCons s
contaCons (App s1 s2) = contaCons s1 + contaCons s2

instance Show a => Show (Seq a) where
    show x = "<<" ++ mostra x ++ ">>"

mostra :: Show a => Seq a -> String
mostra Nil = ""
mostra (Cons a Nil) = show a
mostra (Cons a s) = show a ++ "," ++ mostra s
mostra (App s1 s2) = mostra s1 ++ "," ++ mostra s2

-- Exercicio 5

type Mat a = [[a]]

getElem :: Mat a -> IO a
getElem mat = do
    let (linhas,colunas) = (length mat, length (head mat))
    randLine <- randomRIO (0,linhas - 1)
    randRow <- randomRIO (0,colunas - 1)
    return $ (mat !! randLine) !! randRow

magic :: Mat Int -> Bool
magic mat = linhasIguaisA n mat && colunasIguaisA n mat && diagonaisIguaisA n mat
    where n = sum (head mat)

linhasIguaisA :: Int -> Mat Int -> Bool
linhasIguaisA n = foldl (\acc l -> sum l == n && acc) True 

colunasIguaisA :: Int -> Mat Int -> Bool
colunasIguaisA n mat = foldl (\acc x -> sum (map (\l -> l !! x) mat) == n && acc) True [0..(length mat - 1)]

diagonaisIguaisA :: Int -> Mat Int -> Bool
diagonaisIguaisA n mat = sum (map (\n -> (mat !! n) !! n) [0..ln]) == n && sum (map (\n -> (mat !! n) !! (ln - n)) [0..ln]) == n
    where ln = length mat - 1