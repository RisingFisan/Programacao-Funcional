data Equipamento = Bom | Razoavel | Avariado
    deriving Show

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (Bom:t) = 1 + naoReparar t
naoReparar (Razoavel:t) = 1 + naoReparar t
naoReparar (Avariado:t) = naoReparar t

-- ou

naoReparar' :: [Equipamento] -> Int
naoReparar' [] = 0
naoReparar' (h:t) = case h of Avariado -> naoReparar' t
                              _ -> 1 + naoReparar' t