posPares :: [a] -> [a]
posPares [] = []
posPares [x] = [x]
posPares (h:s:t) = h : posPares t