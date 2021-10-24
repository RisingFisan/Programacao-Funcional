powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n 1 = [1]
powerEnumFrom n m
    | m > 1 = powerEnumFrom n (m - 1) ++ [n^(m-1)]
    | otherwise = []
-- Restringimos `m` para valores naturais, visto que o valor mínimo que este pode tomar é 1.