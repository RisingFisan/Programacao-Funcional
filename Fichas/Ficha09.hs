module Ficha09 where

import System.Random
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Text as Text
import Control.Monad

bingo :: IO()
bingo = do nl <- acumularNumeros []
           print nl

acumularNumeros :: [Int] -> IO [Int]
acumularNumeros l | length l == 90 = do return l
                  | otherwise = do v <- randomRIO (1,90)
                                   print v
                                   getChar
                                   let nl = if v `elem` l then l else v:l in acumularNumeros nl

mastermind :: IO ()
mastermind = do (n1,n2,n3,n4) <- getKey
                doGuess (n1,n2,n3,n4)
                return ()
                                

getKey :: IO (Int,Int,Int,Int)
getKey = do a <- randomRIO (0,9)
            b <- randomRIO (0,9)
            c <- randomRIO (0,9)
            d <- randomRIO (0,9)
            return (a,b,c,d)

getGuess :: IO (Int,Int,Int,Int)
getGuess = do x <- getLine
              if length x /= 4 || not $ all isDigit x
              then getGuess
              else return (let (a:b:c:d:resto) = x in (read [a],read [b],read [c],read [d]))

doGuess :: (Int,Int,Int,Int) -> IO ()
doGuess (n1,n2,n3,n4) = do
    let listaNums = [n1,n2,n3,n4]
    (g1,g2,g3,g4) <- getGuess
    let numsC = 0 + (if n1 == g1 then 1 else 0) + (if n2 == g2 then 1 else 0) + (if n3 == g3 then 1 else 0) + (if n4 == g4 then 1 else 0)
    let numsS = 0 + (if n1 /= g1 && g1 `elem` (listaNums \\ [g2,g3,g4]) then 1 else 0) + 
                    (if n2 /= g2 && g2 `elem` (listaNums \\ [g3,g4]) then 1 else 0) + 
                    (if n3 /= g3 && g3 `elem` (listaNums \\ [g4]) then 1 else 0) + 
                    (if n4 /= g4 && g4 `elem` (listaNums \\ []) then 1 else 0)
    if numsC == 4 then print "Ganhaste, parabens!" else print $ "Valores corretos: " ++ show numsC ++ "   Valores no sitio errado: " ++ show numsS
    if numsC == 4 then return () else doGuess (n1,n2,n3,n4)

-- Exercicio 2

data Aposta = Ap [Int] (Int,Int) deriving Show
    
valida :: Aposta -> Bool
valida (Ap (a:b:c:d:e:[]) (f,g)) = and [x `elem` [1..50] | x <- [a,b,c,d,e]] && all (\x -> x `elem` [1..12]) [f,g]
valida _ = False

comuns :: Aposta -> Aposta -> (Int,Int)
comuns (Ap a1@(a:b:c:d:e:[]) (f,g)) (Ap a2@(h:i:j:k:l:[]) (m,n)) = (contaNums,contaEstr)
    where contaNums = length [x | x <- a1, x `elem` a2]
          contaEstr = length [y | y <- [f,g], y `elem` [m,n]]

instance Eq Aposta where
    (==) a b = comuns a b == (5,2) 

premio :: Aposta -> Aposta -> Maybe Int
premio ap ch = case comuns ap ch of (5,n) -> Just (3 - n)
                                    (4,n) -> Just (6 - n)
                                    (3,n) -> Just (10 - n - (if n == 2 then 1 else 0))
                                    (2,2) -> Just 8
                                    (1,2) -> Just 11
                                    (2,n) -> Just (13 - n)
                                    _ -> Nothing

leAposta :: IO Aposta
leAposta = do
    print "Introduza os numeros (separados por um espaco):"
    nums <- getLine
    print "Introduza as estrelas (separadas por um espaco):"
    stars <- getLine
    let bet = Ap (map read (unspace nums)) (let (a:b:r) = unspace stars in (read a, read b))
    if valida bet then return bet else do print "Aposta invalida, tente novamente!"; leAposta

unspace :: String -> [String]
unspace str = map Text.unpack (Text.split (==' ') (Text.pack str))

joga :: Aposta -> IO ()
joga ch = do
    ap <- leAposta
    print ((++) "Premio: " $ show $ fromMaybe 0 (premio ap ch))

geraChave :: IO Aposta
geraChave = do
    nums <- generate 'N' []
    [star1,star2] <- generate 'S' []
    return (Ap nums (star1,star2))

generate :: Char -> [Int] -> IO [Int]
generate c l = do
    n <- randomRIO (1,if c == 'N' then 50 else 12)
    if length l == 5 && c == 'N' || length l == 2 && c == 'S' then return l 
    else if n `elem` l then generate c l else generate c (n:l)

main :: IO ()
main = do ch <- geraChave
          ciclo ch

ciclo :: Aposta -> IO ()
ciclo ch = do
    menuOpt <- menu
    case menuOpt of "1" -> do joga ch; ciclo ch
                    "2" -> do putStrLn "Nova chave gerada"; main
                    "0" -> return ()


menu :: IO String
menu = do putStrLn menutxt
          putStr "Opcao: "
          getLine
    where menutxt = unlines ["",
                             "Apostar ........... 1",
                             "Gerar nova chave .. 2",
                             "",
                             "Sair .............. 0"]