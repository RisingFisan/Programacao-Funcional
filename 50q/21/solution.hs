isPrime :: Int -> Bool
isPrime n
    | n >= 2 = primeCheck n 2
    | otherwise = False

primeCheck :: Int -> Int -> Bool
primeCheck n m
    | m * m > n = True -- equivalente a: m > √n (assim trabalhamos apenas com valores inteiros)
    | mod n m == 0 = False
    | otherwise = primeCheck n (m + 1)
-- `primeCheck` percorre os números de 2 a √n e verifica se algum destes divide n com resto 0.
-- Caso tal não se verifique para nenhum destes valores, n é primo.