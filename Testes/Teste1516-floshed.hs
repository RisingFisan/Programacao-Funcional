-- Exercicio 1

nub :: Eq a => [a] -> [a]
nub = foldl (\acc x -> if x `elem` acc then acc else acc ++ [x]) []

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f l1 l2 = foldl (\acc n -> acc ++ [f (l1 !! n) (l2 !! n)]) [] [0..(min (length l1) (length l2) - 1)]

zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' _ [] _ = []
zipWith'' _ _ [] = []
zipWith'' f (h:t) (a:b) = f h a : zipWith'' f t b

-- Exercicio 2

type MSet a = [(a,Int)]

converte :: Eq a => [a] -> MSet a
converte l = foldl (\acc x -> if x `elem` map fst acc then map (\(e,n) -> if e == x then (e,n+1) else (e,n)) acc else acc ++ [(x,1)]) [] l

intersect :: Eq a => MSet a -> MSet a -> MSet a
intersect [] _ = []
intersect _ [] = []
intersect ((e,n):r) ms2 | e `elem` map fst ms2 = (e,(min n n2)) : intersect r ms2
                        | otherwise = intersect r ms2
    where n2 = foldl (\acc (a,b) -> if a == e then b else acc) 0 ms2

-- Exercicio 3

data Prop = Var String | Not Prop | And Prop Prop | Or Prop Prop

pr1 :: Prop
pr1 = Not (Or (And (Not (Var "A")) (Var "B")) (Var "C"))

instance Show Prop where
    show (Var str) = str
    show (Not p) = "-" ++ show p
    show (And p1 p2) = "(" ++ show p1 ++ " /\\ " ++ show p2 ++ ")"
    show (Or p1 p2) = "(" ++ show p1 ++ " \\/ " ++ show p2 ++ ")"

eval :: [(String,Bool)] -> Prop -> Bool
eval l (Not p) = not (eval l p)
eval l (Var str) | str `elem` map fst l = foldl (\acc (s,b) -> if s == str then b else acc) True l
                 | otherwise = error ("Valor de variável '" ++ str ++ "' não encontrado.")
eval l (And p1 p2) = (eval l p1) && (eval l p2)
eval l (Or p1 p2) = eval l p1 || eval l p2

nnf :: Prop -> Prop
nnf (Not (And p1 p2)) = (Or (nnf (Not p1)) (nnf (Not p2)))
nnf (Not (Or p1 p2)) = (And (nnf (Not p1)) (nnf (Not p2)))
nnf (Not (Not p)) = p
nnf p = p

avalia :: Prop -> IO Bool
avalia p = do
    let vars = getVars p
    varsValue <- getValues vars
    return $ eval varsValue p 

getVars :: Prop -> [String]
getVars (Var str) = [str]
getVars (Not p) = getVars p
getVars (Or p1 p2) = concatMap (getVars) [p1,p2]
getVars (And p1 p2) = concatMap (getVars) [p1,p2]

getValues :: [String] -> IO [(String,Bool)]
getValues [] = return []
getValues (h:t) = do
    putStr $ "Qual o valor de " ++ h ++ "? "
    valor <- getLine
    resto <- getValues t
    if any (`elem` valor) "vVtT" then return ((h,True):resto) else return ((h,False):resto)