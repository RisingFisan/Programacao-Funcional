import System.Random

-- Exercício 1

inits :: [a] -> [[a]]
inits [] = [[]]
inits l = inits (init l) ++ [l]

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (h:t) (h':t') = h == h' && isPrefixOf t t'

-- Exercício 2

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

tree :: BTree Int
tree = Node 5 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 8 (Node 7 (Node 6 Empty Empty) Empty) (Node 9 Empty Empty))

{-
       5
     /   \  
    /     \
   2       8
  / \     / \
 1   3   7   9
        / 
       6
-}

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node _ l r) = folhas l + folhas r

path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node a _ _) = [a]
path (h:t) (Node a l r) = a:path t (if h then r else l)

-- Exercício 3

type Polinomio = [Coeficiente]
type Coeficiente = Float

pol :: Polinomio
pol = [0,0,0,-5,0,2]

valor :: Polinomio -> Float -> Float
valor p x = foldr (\e -> (+) $ snd e * x ^ fst e) 0 (zip [0..] p)

valor' :: Polinomio -> Float -> Float
valor' p x = sum (zipWith (\a b -> b * x ^ a) [0..] p)

deriv :: Polinomio -> Polinomio
deriv = tail . zipWith (*) [0..]

soma :: Polinomio -> Polinomio -> Polinomio
soma = zipWith (+)

-- Exercício 4

type Mat a = [[a]]

ex = [[1,4,3,2,5],[6,7,8,9,0],[3,5,4,9,1]]

quebraLinha :: [Int] -> [a] -> [[a]]
quebraLinha [] _ = []
quebraLinha (h:t) l = take h l : quebraLinha t (drop h l)

quebraLinhas :: [Int] -> Mat a -> [Mat a]
quebraLinhas [] _ = []
quebraLinhas (h:t) m = map (take h) m : quebraLinhas t (map (drop h) m)

fragmenta :: [Int] -> [Int] -> Mat a -> [Mat a]
fragmenta [] _ _ = []
fragmenta (h:t) pc m = quebraLinhas pc (take h m) ++ fragmenta t pc (drop h m)

geraLinha :: Int -> (Int, Int) -> IO [Int]
geraLinha 0 _ = return []
geraLinha x r = fmap (flip (:)) (geraLinha (x-1) r) <*> randomRIO r
{-
geraLinha x r = do
    t <- geraLinha (x-1) r
    randomRIO r >>= (return . flip (:) t)
-}

{-
geraLinha x r = do
    t <- geraLinha (x-1) r
    n <- randomRIO r
    return $ n : t
-}

geraMat :: (Int, Int) -> (Int, Int) -> IO (Mat Int)
geraMat (0,_) _ = return []
geraMat (x,y) r = fmap (flip (:)) (geraMat (x-1,y) r) <*> geraLinha y r

{-
geraMat (x,y) r = do
    m <- geraMat (x-1,y) r
    l <- geraLinha y r
    return $ l : m
-}