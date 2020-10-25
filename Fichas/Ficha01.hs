module Ficha01 where
import Data.Char
-- Exercício 1

perimetro' :: Double -> Double
perimetro' r = 2 * pi * r

dist' :: (Num a, Eq a, Floating a) => (a,a) -> (a,a) -> a
dist' (x1, y1) (x2, y2) = sqrt(dx ^ 2 + dy ^ 2) where dx = x1 - x2; dy = y1 - y2
                        
primUlt :: [a] -> (a, a)
primUlt l = (head l, last l)

multiplo :: Int -> Int -> Bool
multiplo x y = mod x y == 0

multiplo' :: Int -> Int -> Bool
multiplo' x = (==) 0 . mod x

multiplo'' :: Int -> Int -> Bool
multiplo'' = curry ((== 0) . uncurry mod)

truncaImpar l = if even $ length l then l else tail l 

max2 :: Ord p => p -> p -> p
max2 x y = if x > y then x else y

max3 :: Ord p => p -> p -> p -> p
max3 x y = max2 (max2 x y)

-- Exercício 2

nRaizes a b c 
    | delta > 0 = 2 
    | delta == 0 = 1
    | delta < 0 = 0 
    where delta = b^2 - 4*a*c

raizes a b c 
    | n == 2 = [x1, x2] 
    | n == 1 = [x1] 
    | n == 0 = [] 
    where n = nRaizes a b c
          delta = b^2 - 4*a*c
          (x1,x2) = (((-b) + sqrt delta)/ (2*a), ((-b) - sqrt delta)/ (2*a))

-- Exercício 3

-- type Hora = (Int, Int)

-- horaValida :: Hora -> Bool

horaValida (h, m) = elem h [0..23] && elem m [0..59]

horaMaior (h1, m1) (h2, m2) = if horaValida (h1, m1) && horaValida (h2, m2)
                                  then if h1 > h2 || (h1 == h2 && m1 > m2)
                                           then (h1, m1) 
                                           else (h2, m2) 
                                  else error "Hora inválida!"

-- Versão mais pequena da função acima:
horaMaiorv2 h1 h2 = if h1 > h2 then h1 else h2

hor2min (h, m) = 60 * h + m
min2hor min = (div min 60, mod min 60)

hordiff h1 h2 = min2hor (abs (hor2min h1 - hor2min h2))

addmins (h, m) min = min2hor (mod (hor2min (h, m) + min) 1440)

-- Exercício 4

data Hora = H Int Int deriving (Show, Eq)

horaValida' :: Hora -> Bool
horaValida' (H h m) = elem h [0..23] && elem m [0..59]

(H h m) `horaDepois` (H h2 m2) = (h,m) >= (h2,m2)

hor2min' (H h m) = 60 * h + m

min2hor' min = H (div min 60) (mod min 60)

hordiff' h1 h2 = min2hor' $ abs $ hor2min' h1 - hor2min' h2

addmins' hor min = min2hor' $ mod (hor2min' hor + min) 1440

-- Exercício 5

data Semaforo = Verde | Amarelo | Vermelho deriving (Show, Eq)

next :: Semaforo -> Semaforo
next s = case s of Verde -> Amarelo
                   Amarelo -> Vermelho
                   Vermelho -> Verde

stop :: Semaforo -> Bool
stop s
    | s == Vermelho = True
    | otherwise = False

safe :: Semaforo -> Semaforo -> Bool
safe s1 s2 = s1 == Vermelho || s2 == Vermelho

-- Exercício 6

data Ponto = Cartesiano Double Double | Polar {distPonto :: Double, anguloPonto :: Double} deriving (Show,Eq)

posx :: Ponto -> Double
posx ponto = case ponto of Cartesiano x _ -> x
                           Polar d a -> if a == pi/2 then 0 else d * cos a

posy :: Ponto -> Double
posy ponto = case ponto of Cartesiano _ y -> y
                           Polar d a -> if a == pi then 0 else d * sin a

raio :: Ponto -> Double
raio ponto = case ponto of Cartesiano x y -> sqrt $ x^2 + y^2
                           Polar d _ -> d

angulo :: Ponto -> Double
angulo ponto = case ponto of Cartesiano x y -> if x < 0 && y == 0 then pi else
                                               if x < 0 then pi + atan (y/x) else
                                               atan (y/x)
                             Polar _ a -> a

dist :: Ponto -> Ponto -> Double
dist ponto1 ponto2 = sqrt (((posx ponto1 - posx ponto2) ^ 2) + (posy ponto1 - posy ponto2) ^ 2)

-- Exercício 7

data Figura = Circulo Ponto Double | Retangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show, Eq)

poligono :: Figura -> Bool
poligono (Circulo c r) = False
poligono (Retangulo p1 p2) = posx p1 /= posx p2 && posy p1 /= posy p2 -- Verifica que os pontos não têm o mesmo valor de x ou y
poligono (Triangulo p1 p2 p3) = (posy p2 - posy p1) / (posx p2 - posx p1) /= (posy p3 - posy p2) / (posx p3 - posx p2) -- Verifica que os pontos não pertencem todos à mesma reta

vertices :: Figura -> [Ponto]
vertices (Circulo _ _) = []
vertices retang@(Retangulo p1 p2) = if poligono retang then [p1, Cartesiano (posx p1) (posy p2), Cartesiano (posx p2) (posy p1), p2] else []
vertices triang@(Triangulo p1 p2 p3) = if poligono triang then [p1, p2, p3] else []

area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = dist p1 p2
        b = dist p2 p3
        c = dist p3 p1
        s = (a+b+c) / 2 -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) -- fórmula de Heron
area (Circulo _ r) = pi * (r ^ 2)
area (Retangulo p1 p2) = abs (posx p2 - posx p1) * abs (posy p2 - posy p1) 

perimetro :: Figura -> Double
perimetro (Circulo _ r) = 2 * pi * r
perimetro (Retangulo p1 p2) = 2 * abs (posx p2 - posx p1) + 2 * abs (posy p2 - posy p1)
perimetro (Triangulo p1 p2 p3) = dist p1 p2 + dist p2 p3 + dist p1 p3

-- Exercicio 8

isLower' :: Char -> Bool
isLower' ch = ch `elem` ['a'..'z']

isUpper' :: Char -> Bool
isUpper' ch = ch `elem` ['A'..'Z']

isDigit' :: Char -> Bool
isDigit' d = d `elem` ['0'..'9']

isAlpha' :: Char -> Bool
isAlpha' ch = isLower' ch || isUpper' ch

toUpper' :: Char -> Char
toUpper' ch = if isLower' ch then chr (ord ch - 32) else ch

intToDigit' :: Int -> Char
intToDigit' n = chr (n + 48)

digitToInt' :: Char -> Int 
digitToInt' ch = ord ch - 48
