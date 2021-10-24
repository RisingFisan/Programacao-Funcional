data Movimento = Norte | Sul | Este | Oeste deriving Show

caminho :: (Int, Int) -> (Int, Int) -> [Movimento]
caminho (xi, yi) (xf, yf) 
    | xi < xf = Este : caminho (xi + 1, yi) (xf, yf)
    | xi > xf = Oeste : caminho (xi - 1, yi) (xf, yf)
    | yi < yf = Norte : caminho (xi, yi + 1) (xf, yf)
    | yi > yf = Sul : caminho (xi, yi - 1) (xf, yf)
    | otherwise = []