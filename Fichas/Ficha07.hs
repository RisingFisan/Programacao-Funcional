-- Exercicio 1

data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt

calcula :: ExpInt -> Int
calcula (Const num) = num
calcula (Simetrico exp) = (- calcula exp)
calcula (Mais a b) = calcula a + calcula b
calcula (Menos a b) = calcula a - calcula b
calcula (Mult a b) = calcula a * calcula b

infixa :: ExpInt -> String
infixa (Const num) = show num
infixa (Simetrico exp) = "(-" ++ infixa exp ++ ")"
infixa (Mais a b) = '(':infixa a ++ "+" ++ infixa b ++ ")"
infixa (Menos a b) = '(':infixa a ++ "-" ++ infixa b ++ ")"
infixa (Mult a b) = '(':infixa a ++ "*" ++ infixa b ++ ")"

posfixa :: ExpInt -> String
posfixa (Const num) = show num ++ " "
posfixa (Simetrico exp) = '-':posfixa exp
posfixa (Mais a b) = posfixa a ++ posfixa b ++ "+ "
posfixa (Menos a b) = posfixa a ++ posfixa b ++ "- "
posfixa (Mult a b) = posfixa a ++ posfixa b ++ "* "

-- Exercicio 2

data RTree a = R a [RTree a] deriving Show

rtree1 = R 6 [R 4 [R 7 [R 1 [],
                        R 3 []],
                   R 9 []],
              R 3 [R 12 []],
              R 6 [],
              R 11 []]

soma :: Num a => RTree a -> a
soma (R e []) = e
soma (R e es) = e + sum (map (soma) es)

altura :: RTree a -> Int
altura (R e []) = 1
altura (R e es) = 1 + maximum (map altura es)

prune :: Int -> RTree a -> RTree a
prune 0 (R e es) = R e []
prune n (R e es) = R e (map (prune (n - 1)) es)

mirror :: RTree a -> RTree a
mirror (R e es) = R e (map mirror (reverse es))

postorder :: RTree a -> [a]
postorder (R e []) = [e]
postorder (R e es) = concatMap postorder es ++ [e]

-- Exercicio 3

data LTree a = Tip a | Fork (LTree a) (LTree a) deriving Show

ltree1 = Fork (Fork (Tip 5)
                    (Fork (Tip 6)
                          (Tip 4)))
              (Fork (Fork (Tip 3)
                          (Tip 7))
                    (Tip 5))

ltSum :: Num a => LTree a -> a
ltSum (Tip n) = n
ltSum (Fork a b) = ltSum a + ltSum b 

listaLT :: LTree a -> [a]
listaLT (Tip n) = [n]
listaLT (Fork a b) = listaLT a ++ listaLT b

ltHeight :: LTree a -> Int
ltHeight (Tip _) = 0
ltHeight (Fork a b) = 1 + max (ltHeight a) (ltHeight b)

-- Exercicio 4

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show
data FTree a b = Leaf b | No a (FTree a b) (FTree a b) deriving Show

ftree1 = No 8 (No 1 (Leaf 5)
                    (No 2 (Leaf 6)
                          (Leaf 4)))
              (No 9 (No 10 (Leaf 3)
                           (Leaf 7))
                    (Leaf 5))

splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf n) = (Empty, Tip n)
splitFTree (No a b c) = (Node a (fst (splitFTree b)) (fst (splitFTree c)), Fork (snd (splitFTree b)) (snd (splitFTree c)))

joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees (Empty) (Tip n) = Just (Leaf n)
joinTrees (Node e l r) (Fork a b) = Just (No e aux aux2)
    where Just aux = joinTrees l a
          Just aux2 = joinTrees r b
joinTrees _ _ = Nothing