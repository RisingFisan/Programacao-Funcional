data ArvIrr a = No a [ArvIrr a] deriving Show

arv1 = No 2 [No 3 [No 6 []], No 4 [No 7 [], No 8[]], No 5[]]

elems :: ArvIrr a -> [(a,Int)]
elems a = elemsAux 1 a
    where elemsAux n (No x l) = (x,n):concatMap (elemsAux (n+1)) l

unElems :: [(a,Int)] -> ArvIrr a
unElems ((no,_):l) = No no (map unElems (filteredList l))
    where filteredList :: [(a,Int)] -> [[(a,Int)]]
          filteredList [] = []
          filteredList ((h,n):t) = ((h,n):takeWhile ((n <) . snd) t) : filteredList (dropWhile ((n <) . snd) t)