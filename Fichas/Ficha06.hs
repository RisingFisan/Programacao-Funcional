data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

arvore1 = (Node 5 (Node 2 (Node 1 Empty
                                  Empty) 
                          (Node 3 Empty 
                                  Empty)) 
                  (Node 9 (Node 7 (Node 6 Empty 
                                          Empty) 
                                  (Node 8 Empty 
                                          Empty)) 
                          Empty))

-- Exercicio 1

altura :: BTree a -> Int
altura Empty = 0
altura (Node _ a b) = max (1 + altura a) (1 + altura b)

contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node _ a b) = 1 + contaNodos a + contaNodos b

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node _ a b) = folhas a + folhas b

prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 _ = Empty
prune x (Node e l r) = Node e (prune (x - 1) l) (prune (x - 1) r)

path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node e l r) = [e]
path (h:t) (Node e l r) = e : path t (if h then r else l)  

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node e l r) = Node e (mirror r) (mirror l)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node e l r) (Node a b c) = Node (f e a) (zipWithBT f l b) (zipWithBT f r c)
zipWithBT _ _ _ = Empty

unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (a,b,c) l r) = (Node a unzipL1 unzipR1,Node b unzipL2 unzipR2,Node c unzipL3 unzipR3)
    where (unzipL1,unzipL2,unzipL3) = unzipBT l
          (unzipR1,unzipR2,unzipR3) = unzipBT r

-- Exercicio 2

minimo :: (Ord a) => BTree a -> a
minimo (Node e Empty _) = e
minimo (Node e l r) = minimo l

semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node _ Empty _) = Empty
semMinimo (Node e l r) = Node e (semMinimo l) r

minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node e Empty _) = (e,Empty)
minSmin (Node e l r) = (a,Node e b r)
    where (a,b) = minSmin l

remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x (Node e l r) | x < e = Node e (remove x l) r
                      | x > e = Node e l (remove x r)
                      | otherwise = aux x (Node e l r)
    where aux n (Node a b c) = case b of Empty -> c
                                         otherwise -> case c of Empty -> b
                                                                otherwise -> Node g b h
          (g,h) = minSmin r

-- Exercicio 3

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL  deriving Show
data Classificacao = Aprov Int | Rep | Faltou deriving Show
type Turma = BTree Aluno  --  árvore binária de procura (ordenada por número)

turma1 :: Turma
turma1 = (Node (15,"Luís",ORD,Aprov 14) (Node (12,"Joana",MEL,Faltou) (Node (7,"Diogo",TE,Rep) Empty
                                                                                               Empty) 
                                                                      (Node (14,"Lara",ORD,Aprov 19) Empty
                                                                                                     Empty))
                                        (Node (20,"Pedro",TE,Aprov 10) Empty
                                                                       (Node (25,"Sofia",ORD,Aprov 20) (Node (23,"Rita",ORD,Aprov 17) Empty
                                                                                                                                      Empty)
                                                                                                       (Node (28,"Vasco",MEL,Rep) Empty
                                                                                                                                  Empty))))

inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum n (Node (num,_,_,_) l r) = n == num || inscNum n (if n < num then l else r)

inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome n (Node (_,nom,_,_) l r) = n == nom || inscNome n l || inscNome n r

trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,nom,reg,_) l r) = (case reg of TE -> [(num,nom)];otherwise -> []) ++ trabEst l ++ trabEst r

nota :: Numero -> Turma -> Maybe Classificacao
nota n (Node (num,_,_,clas) l r) | n == num = Just clas
                                       | n < num = nota n l
                                       | otherwise = nota n r
nota _ _ = Nothing

percFaltas :: Turma -> Float
percFaltas Empty = 0
percFaltas turma = sumFaltas turma / numAlunos turma * 100
    where sumFaltas Empty = 0
          sumFaltas (Node (_,_,_,clas) l r) = (case clas of Faltou -> 1;otherwise -> 0) + sumFaltas l + sumFaltas r
          numAlunos Empty = 0
          numAlunos (Node e l r) = 1 + numAlunos l + numAlunos r

mediaAprov :: Turma -> Float
mediaAprov Empty = 0
mediaAprov turma = sumNotas turma / numNotas turma
    where sumNotas :: Turma -> Float
          sumNotas Empty = 0
          sumNotas (Node (_,_,_,Aprov nota) l r) = fromIntegral nota + sumNotas l + sumNotas r
          sumNotas (Node e l r) = sumNotas l + sumNotas r
          numNotas :: Turma -> Float
          numNotas (Node (_,_,_,clas) l r) = (case clas of Aprov nota -> 1;otherwise -> 0) + numNotas l + numNotas r
          numNotas _ = 0

aprovAv :: Turma -> Float
aprovAv Empty = 0
aprovAv turma = a / b
    where (a,b) = aux turma
          aux Empty = (0,0)
          aux (Node (_,_,_,clas) l r) = case clas of Aprov nota -> (x+1,y) ; Rep -> (x,y+1) ; otherwise -> (x,y)
            where (x,y) = (c+e,d+f)
                  (c,d) = aux l
                  (e,f) = aux r
