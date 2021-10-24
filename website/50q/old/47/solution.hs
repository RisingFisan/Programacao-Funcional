data Posicao = Pos Int Int deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral [Pos x y] = Pos x y
maisCentral ((Pos x y):(Pos a b):ps) = if (x^2 + y^2) < (a^2 + b^2) then maisCentral (Pos x y : ps) else maisCentral (Pos a b : ps)