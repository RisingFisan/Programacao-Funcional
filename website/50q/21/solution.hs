isPrime :: Int -> Bool
isPrime 2 = True
isPrime n
    | n > 2 = primeCheck n 2
    | otherwise = False

primeCheck :: Int -> Int -> Bool
primeCheck n m
    | fromIntegral m > sqrt (fromIntegral n) = True
    | mod n m == 0 = False
    | otherwise = primeCheck n (m + 1)
-- `primeCheck` percorre os números de 2 a `sqrt n` e verifica se algum destes divide n com resto 0.
-- Caso tal não se verifique para nenhum destes valores, n é primo.