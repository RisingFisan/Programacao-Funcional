-- Exercicio 1

unlines' ::  [String] -> String
unlines' = foldl (\acc -> flip (++) ['\n'] . (++) acc) []

(\\) :: (Eq a) => [a] -> [a] -> [a]
(\\) l r = foldl (\acc x -> let (a,b) = span (/= x) acc in a ++ (if null b then [] else tail b)) l r

-- Exercicio 2

data Seq a = Nil | Inicio a (Seq a) | Fim (Seq a) a

primeiro ::  Seq a -> a
primeiro (Inicio a s) = a
primeiro (Fim Nil a) = a
primeiro (Fim s a) = primeiro s

semUltimo :: Seq a -> Seq a
semUltimo (Inicio a Nil) = Nil
semUltimo (Inicio a s) = Inicio a (semUltimo s)
semUltimo (Fim s a) = s

-- Exercicio 3

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 _ = Empty
prune n (Node x l r) = Node x (prune n l) (prune n r)

semMinimo :: (Ord a) => BTree a -> BTree a
semMinimo Empty = Empty
semMinimo (Node _ Empty r) = r
semMinimo (Node x l r) = Node x (semMinimo l) r

-- Exercicio 4

type Tabuleiro = [String]
exemplo :: Tabuleiro
exemplo = ["..R.",
           "R...",
           "...R",
           ".R.."]

posicoes :: Tabuleiro -> [(Int,Int)]
posicoes tab = foldl (\acc y -> acc ++ (foldl (\acc2 x -> if (tab !! y) !! x == 'R' then acc2 ++ [(x,y)] else acc2)) [] [0..(length (head tab) - 1)]) [] [0..(length tab - 1)]

valido :: Tabuleiro -> Bool
valido tab = foldl (\acc (x,y) -> if length (filter (\(a,b) -> (a,b) /= (x,y) && (a == x || b == y || a - b == x - y || b - a == y - x)) (posicoes tab)) > 0 then False else acc) True (posicoes tab)

bemFormado ::  Int -> Tabuleiro -> Bool
bemFormado n tab = length tab == n && foldr (\x -> (&&) $ (==) n $ length x) True tab && foldl (\acc (x,y) -> if (tab !! y) !! x == 'R' then acc + 1 else acc) 0 [(a,b) | a <- [0..n - 1], b <- [0..n - 1]] == n