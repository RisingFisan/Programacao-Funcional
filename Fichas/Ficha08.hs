module Ficha08 where

import Data.List
import Data.Char

-- Exercicio 1

data Frac = F Integer Integer

instance Eq Frac where
    (F a b) == (F c d) = a * d == c * b

instance Show Frac where
    show (F a b) = show a ++ "/" ++ show b

instance Num Frac where
    -- (+), (*), (-) :: a -> a -> a
    -- negate, abs, signum :: a -> a
    -- fromInteger :: Integer -> a
    (F a b) + (F c d) | b == d = normaliza $ F (a + c) b
                      | otherwise = normaliza $ F (a * d + b * c) (b * d)
    x - y = x + negate y
    (F a b) * (F c d) = F (a * c) (b * d)
    negate (F a b) = F (-a) b
    abs (F a b) = F (abs a) (abs b)
    signum (F a b) | a == 0 = 0
                   | a * b > 0 = 1
                   | otherwise = -1
    fromInteger x = F x 1

instance Ord Frac where
    (F a b) <= (F c d) = a * d <= c * b

normaliza :: Frac -> Frac
normaliza (F a b) = F (aX `div` x) (bX `div` x)
    where x = mdc (abs a) (abs b) * (if a * b < 0 then (-1) else 1)
          aX | a < 0 = -a
             | otherwise = a
          bX | a < 0 = -b
             | otherwise = b

mdc :: Integer -> Integer -> Integer
mdc a b = last [n | n <- [1..(min a b)] , a `mod` n == 0, b `mod` n == 0]

maioresQueDobro :: Frac -> [Frac] -> [Frac]
maioresQueDobro = filter . (<) . (2 *)

-- Exercicio 2

data Exp a = Const a
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

instance Show a => Show (Exp a) where
    show (Const a) = show a
    show (Simetrico a) = "(- " ++ show a ++ ")"
    show (Mais a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Menos a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
    show (Mult a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
    
valorDe :: (Num a) => Exp a -> a
valorDe (Const a) = a
valorDe (Simetrico a) = - (valorDe a)
valorDe (Mais a b) = valorDe a + valorDe b
valorDe (Menos a b) = valorDe a - valorDe b
valorDe (Mult a b) = valorDe a * valorDe b

instance (Num a,Eq a) => Eq (Exp a) where
    x == y = valorDe x == valorDe y

instance (Num a, Eq a) => Num (Exp a) where
    x + y = Const (valorDe x + valorDe y)
    --
    x - y = Const (valorDe x - valorDe y)
    --
    x * y = Const (valorDe x * valorDe y)
    --
    negate (Const a) = Const (- a)
    negate (Simetrico a) = a
    negate (Mais a b) = Mais (- a) (- b)
    negate (Menos a b) = Menos b a
    negate (Mult a b) = Mult (-a) b
    fromInteger x = Const (fromInteger x)
    abs (Const a) = Const (abs a)
    abs (Simetrico a) = abs a
    abs (Mais a b) = abs (a + b)
    abs (Menos a b) = abs (a - b)
    abs (Mult a b) = abs (a * b)
    signum (Const a) = Const (if abs a == a then if a == 0 then 0 else 1 else (-1))
    signum (Simetrico a) = - signum a
    signum (Mais a b) = Const (if abs (a + b) == a + b then if a + b == 0 then 0 else 1 else (-1))
    signum (Menos a b) = Const (if abs (a - b) == a - b then if a - b == 0 then 0 else 1 else (-1))
    signum (Mult a b) = Const (if abs (a * b) == a * b then if a * b == 0 then 0 else 1 else (-1))

-- Exercicio 3

data Movimento = Credito Float | Debito Float
data Data = D Int Int Int deriving Eq
data Extracto = Ext Float [(Data, String, Movimento)]

instance Ord Data where
    compare (D dia1 mes1 ano1) (D dia2 mes2 ano2) | ano1 > ano2 || ano1 == ano2 && (mes1 > mes2 || mes1 == mes2 && dia1 > dia2) = GT
                                                  | ano1 == ano2 && mes1 == mes2 && dia1 == dia2 = EQ
                                                  | otherwise = LT

instance Show Data where 
    show (D dia mes ano) = intercalate "/" $ map show [dia,mes,ano]

ordena :: Extracto -> Extracto
ordena (Ext n l) = Ext n (sortBy (\(data1,_,_) (data2,_,_) -> compare data1 data2) l)

instance Show Extracto where
    show (Ext n l) = "Saldo anterior: " ++ show n ++
                     "\n---------------------------------------" ++
                     "\nData       Descricao   Credito   Debito" ++
                     "\n---------------------------------------\n" ++ concatMap (\(dat,str,_) -> show dat ++ replicate (11 - length (show dat)) ' ' ++ map toUpper str ++ "    \n") l ++
                     "---------------------------------------" ++
                     "\nSaldo actual: " ++ show (saldo (Ext n l))

saldo :: Extracto -> Float
saldo (Ext x lm) = foldl (\acc (_,_,mov) -> case mov of Credito n -> acc + n
                                                        Debito n -> acc - n) x lm