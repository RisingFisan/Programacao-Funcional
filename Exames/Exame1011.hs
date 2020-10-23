import Data.Maybe

type MSet a = [(a, Int)]

mset1 :: MSet Char
mset1 = [('a',3),('b',1),('c',2)]

moda :: MSet a -> a
moda = fst . foldr1 (\x acc -> if snd x > snd acc then x else acc)

type Rel1 a b = [(a,b)]
type Rel2 a b = [(a,[b])]
type Rel3 a b = ([a], a -> [b])

rel1 = [(1,'a'),(2,'b'),(5,'d'),(1,'e'),(6,'a'),(2,'f')]
rel2 = [(1,['a','e']),(2,['b','f']),(5,['d']),(6,['a'])]

rel12 :: (Eq a) => Rel1 a b -> Rel2 a b
rel12 [] = []
rel12 ((ha,hb):t) = foldl (\(acc,accl) x -> if fst x == acc then (acc,snd x : accl) else (acc,accl)) (ha,[hb]) t : rel12 (filter (\(a,b) -> a /= ha) t)

rel23 :: (Eq a) => Rel2 a b -> Rel3 a b
rel23 l = (map fst l, (Data.Maybe.fromMaybe []) . (flip lookup l))

rel31 :: (Rel3 a b) -> Rel1 a b
rel31 ([],_) = []
rel31 ((h:t),f) =  [(h,x) | x <- f h] ++ rel31 (t,f)

compoe