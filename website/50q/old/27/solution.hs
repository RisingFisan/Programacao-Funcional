unlines :: [String] -> String
unlines [] = ""
unlines (h:t) = h ++ "\n" ++ unlines t