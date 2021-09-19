data Semaforo = Verde | Amarelo | Vermelho deriving Show

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK ss = length [s | s <- ss, case s of Vermelho -> False; _ -> True] < 2