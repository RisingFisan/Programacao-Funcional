total :: [[a]] -> Int
total [] = 0
total (h:t) = subTotal h + total t
    where subTotal :: [a] -> Int
          subTotal [] = 0
          subTotal (h:t) = 1 + subTotal t
-- `subTotal` pode ser substituída pela função pré-definida `length`