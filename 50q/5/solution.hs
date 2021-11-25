reverse :: [a] -> [a]
reverse [] = []
reverse (h:t) = reverse t ++ [h]