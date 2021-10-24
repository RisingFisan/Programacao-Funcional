heads :: [[a]] -> [a]
heads [] = []
heads ([]:t) = heads t -- é necessária esta condição pois `head []` dá erro
heads (h:t) = head h : heads t