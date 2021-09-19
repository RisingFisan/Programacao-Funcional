data Posicao = Pos Int Int deriving Show

mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = True
mesmaOrdenada [Pos _ _] = True
mesmaOrdenada ((Pos _ y):(Pos x2 y2):ps) = y == y2 && mesmaOrdenada (Pos x2 y2 : ps)