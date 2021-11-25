catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (m:ms) = case m of Nothing -> catMaybes ms
                             Just x -> x : catMaybes ms