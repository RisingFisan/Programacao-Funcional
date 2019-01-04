import Data.List ((\\))

-- Exercicio 1

(??) :: [a] -> Int -> a
(??) (h:t) n | n == 0 = h
             | otherwise = (??) t (n - 1)

-- Exercicio 2

data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (mov:movs) = case mov of Norte -> posicao (x,y+1) movs
                                       Este -> posicao (x+1,y) movs
                                       Sul -> posicao (x,y-1) movs
                                       Oeste -> posicao (x-1,y) movs

-- Exercicio 3

any' :: (a -> Bool) -> [a] -> Bool
any' f = foldl (\acc x -> if f x then True else acc) False

-- Exercicio 4

type Mat a = [[a]]

triSup :: (Num a,Eq a) => Mat a -> Bool
triSup matriz = all (\n -> all (\x -> (matriz !! n) !! x == 0) [0..(n - 1)]) [1..(length matriz - 1)]

-- Exercicio 5

movimenta :: IO (Int,Int)
movimenta = moveFrom (0,0)

moveFrom :: (Int,Int) -> IO (Int,Int)
moveFrom (x,y) = do
    dir <- getChar
    case dir of 'n' -> moveFrom (x,y+1)
                's' -> moveFrom (x,y-1)
                'e' -> moveFrom (x+1,y)
                'o' -> moveFrom (x-1,y)
                'w' -> moveFrom (x-1,y)
                otherwise -> return (x,y) 

-- Exercicio 6

data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]

ex = Mover (5,5) (Juntar [Mover (0,1) (Quadrado 5),
                          Quadrado 4,
                          Mover (4,3) (Quadrado 2)])

ex2 = Juntar [Mover (5,5) (Quadrado 4), 
              Mover (5,6) (Quadrado 5),
              Mover (9,8) (Quadrado 2)]

vazia :: Imagem -> Bool
vazia (Quadrado _) = False
vazia (Mover _ img) = vazia img
vazia (Juntar imgs) | null imgs = True
                    | otherwise = or (map vazia imgs)

maior :: Imagem -> Maybe Int
maior (Quadrado n) = Just n
maior (Mover _ img) = maior img
maior (Juntar imgs) | null imgs = Nothing
                    | otherwise = maximum' (filter (/= Nothing) (map maior imgs))
    where maximum' [] = Nothing
          maximum' l = maximum l

instance Eq Imagem where
    img1 == img2 = null $ (quadPos img1 (0,0)) \\ (quadPos img2 (0,0)) 

quadPos :: Imagem -> (Int,Int) -> [(Int,(Int,Int))]
quadPos (Quadrado n) pos = [(n,pos)]
quadPos (Mover (a,b) img) (x,y) = quadPos img (x+a,y+b)
quadPos (Juntar imgs) pos = concatMap (\x -> quadPos x (pos)) imgs