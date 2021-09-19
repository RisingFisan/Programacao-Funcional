posImpares :: [a] -> [a]
posImpares [] = []
posImpares [_] = []
posImpares (h:s:t) = s : posImpares t