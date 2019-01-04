import Data.List (sort)
import System.Random

data Heap a = Vazia | Nodo a (Heap a) (Heap a)

ex1 = Nodo 2 (Nodo 5 (Nodo 10 (Nodo 12 Vazia Vazia) 
                              (Nodo 20 Vazia Vazia)) 
                     (Nodo 6 (Nodo 15 Vazia Vazia) 
                              Vazia)) 
             (Nodo 8 (Nodo 13 Vazia Vazia) 
                     (Nodo 21 Vazia Vazia))


quantos :: Heap a -> Int
quantos Vazia = 0
quantos (Nodo a b c) = 1 + quantos b + quantos c

existe :: Ord a => a -> Heap a -> Bool
existe n (Vazia) = False
existe n (Nodo e l r) | n < e = False
                      | n == e = True
                      | otherwise = existe n l || existe n r

removeMin :: Ord a => Heap a -> (a,Heap a)
removeMin (Nodo e l r) = (e,aux l r)
    where aux Vazia x = x
          aux x Vazia = x
          aux (Nodo a b c) (Nodo g h i) | a < g = Nodo a (aux b c) (Nodo g h i)
                                        | otherwise = Nodo g (Nodo a b c) (aux h i) 

instance (Show a) => Show (Heap a) where
    show Vazia = ""
    show h = mostra 0 h
        where mostra n Vazia = ""
              mostra n (Nodo e Vazia Vazia) = "---" ++ show e
              mostra n (Nodo e l r) = "---" ++ show e ++ mostra novoN l ++ "\n" ++ replicate novoN ' ' ++ mostra novoN r where novoN = n + length (show e) + 3

instance Ord a => Eq (Heap a) where
    Vazia == Vazia = True
    x == y = lista x == lista y
    
lista :: Ord a => Heap a -> [a]
lista Vazia = []
lista (Nodo e l r) = e:sort (lista l ++ lista r)

randomHeap :: Ord a => [a] -> IO (Heap a)
randomHeap [] = do return Vazia
randomHeap l = do
    let sl = sort l
    randNum <- randomRIO (0,length l)
    rl <- randomHeap (take randNum (tail sl))
    rr <- randomHeap (drop randNum (tail sl))
    return $ Nodo (head sl) rl rr