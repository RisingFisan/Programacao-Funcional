data Posicao = Pos Int Int deriving Show

-- Versão sem funções de ordem superior
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos _ [] = []
vizinhos (Pos x y) ((Pos xv yv):ps) = if abs (x - xv) == 1 && y == yv || abs (y - yv) == 1 && x == xv 
                                       then Pos xv yv : vizinhos (Pos x y) ps 
                                       else vizinhos (Pos x y) ps

-- Versão com funções de ordem superior
vizinhos' :: Posicao -> [Posicao] -> [Posicao]
vizinhos' (Pos x y) = filter (\(Pos a b) -> abs (a - x) + abs (b - y) == 1)