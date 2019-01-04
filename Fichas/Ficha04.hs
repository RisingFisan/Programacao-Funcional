module Ficha04 where

import Data.Char (isDigit, isAlpha)

-- Exercicio 1

-- a) [6,12,18]
-- b) [6,12,18]
-- c) [(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)]
-- d) [1,1,4,4,9,9,16,16,25,25]

-- Exercicio 2

doisA = [2^x | x <- [0..10]]
doisB = [(x,y) | x <- [1..5], y <- [1..5], x + y == 6]
doisC = [ [1..x] | x <- [1..5]]
doisD = [ replicate x 1 | x <- [1..5]]
doisE = [ factorial x | x <- [1..6]]
    where factorial 0 = 1
          factorial x = x * factorial (x - 1)
doisE' = [ product [y | y <- [1..x]] | x <- [1..6]]

-- Exercicio 3

digitAlpha :: String -> (String,String)
digitAlpha string = foldl (\(alpha,digit) x -> if isDigit x then (alpha,digit ++ [x]) else if isAlpha x then (alpha ++ [x],digit) else (alpha,digit)) ("","") string

-- Exercicio 4

nzp :: [Int] -> (Int,Int,Int)
nzp = foldl (\(n,z,p) x -> if x < 0 then (n+1,z,p) else if x > 0 then (n,z,p+1) else (n,z+1,p)) (0,0,0)

-- Exercicio 5 

divMod :: Integral a => a -> a -> (a, a)
divMod x y = foldl (\(a,b) n -> (a+1,b-y)) (0,x) [y,2*y..x]

-- Exercicio 6

fromDigits :: [Int] -> Int
fromDigits = foldl (\acc x -> x + 10 * acc ) 0

-- Exercicio 8

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib' 