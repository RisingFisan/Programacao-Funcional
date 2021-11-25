data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao p [] = p
posicao (x,y) (m:ms) = posicao (case m of Norte -> (x, y + 1)
                                          Sul -> (x, y - 1)
                                          Este -> (x + 1, y)
                                          Oeste -> (x - 1, y)) ms