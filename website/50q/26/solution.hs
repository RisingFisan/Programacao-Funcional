unwords :: [String] -> String
unwords [] = ""
unwords (h:t) = h ++ (if null t then "" else " ") ++ unwords t