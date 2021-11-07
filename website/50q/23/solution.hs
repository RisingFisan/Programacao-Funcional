isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf l l'@(_:t) = l == l' || isSuffixOf l t
-- l'@(h:t) significa a lista `l'` com cabeça `h` e cauda `t`
-- seria o mesmo que ter:
-- isSuffixOf l l' = ...
--     where l' = h:t