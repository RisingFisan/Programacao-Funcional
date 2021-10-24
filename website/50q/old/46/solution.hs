data Movimento = Norte | Sul | Este | Oeste deriving Show

vertical :: [Movimento] -> Bool
vertical [] = True
vertical (l:ls) = case l of Este -> False
                            Oeste -> False
                            _ -> vertical ls