import Data.List (nub)

-- Exercício 1

intersect :: Eq a => [a] -> [a] -> [a]
intersect l l2 = foldr (\x acc -> if x `elem` l2 then x:acc else acc) [] l

tails :: [a] -> [[a]]
tails [] = [[]]
tails l = l : tails (tail l)

-- Exercício 2

type ConjInt = [Intervalo]
type Intervalo = (Int, Int)

elems :: ConjInt -> [Int]
elems [] = []
elems ((a, b) : t)
    | a == b    = a : elems t
    | otherwise = a : elems ((succ a, b) : t)

geraconj :: [Int] -> ConjInt
geraconj [] = []
geraconj (h : t) = (h, d) : geraconj (dropWhile (<= d) t)
    where d = foldl (\acc x -> if x == succ acc then x else acc) h t

-- Exercício 3

data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
    deriving (Show)

type Nome = String
type Agenda = [(Nome, [Contacto])]

agenda1 = [("Sofia", [Casa 123456789, Tlm 987654321, Email "abc@def.ghi", Email "f@mendess.xyz"]),("Luís", [Tlm 69420]),("Rita", [Trab 58008])]

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email [] = [(nome, [Email email])]
acrescEmail nome email agenda@((nomeX, contactos) : t)
    | nome == nomeX = (nome, (Email email) : contactos) : t
    | otherwise     = head agenda : acrescEmail nome email t

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails nome [] = Nothing
verEmails nome agenda@((nomeX, contactos) : t)
    | nome == nomeX = foldr (\x acc -> case x of Email email -> if acc == Nothing then Just [email] else ((:) email) <$> acc; otherwise -> acc) Nothing contactos
    | otherwise     = verEmails nome t

consulta :: [Contacto] -> ([Integer],[String])
consulta = foldr (\x (i,s) -> case x of Email email -> (i,email:s); otherwise -> (n x:i,s)) ([],[]) 
    where n x = case x of Casa num -> num
                          Trab num -> num
                          Tlm num -> num

consultaIO :: Agenda -> IO ()
consultaIO agenda = do
    nome <- getLine
    let contactos = aux nome agenda
    putStr (concat [show x ++ "\n" | x <- contactos])

    where aux _ [] = []
          aux nome ((name,contactos):t) = if name == nome then contactos else aux nome t

-- Exercício 4

data RTree a = R a [RTree a] deriving (Show, Eq)

tree1 = R 1 [R 2 [],
             R 3 [R 4 [R 5 [],
                       R 6 []
                      ]
                 ],
             R 7 []
            ]

paths :: RTree a -> [[a]]
paths (R node []) = [[node]]
paths (R node branches) = [ node : x | x <- concat [paths branch | branch <- branches]]

unpaths :: Eq a => [[a]] -> RTree a
unpaths [[x]] = R x []
unpaths list = R n [unpaths (foldl (\acc branch -> if head branch == y then branch:acc else acc) [] branches) | y <- heads]
    where n = head $ head list
          branches = [tail x | x <- list]
          heads = Data.List.nub [head x | x <- branches]