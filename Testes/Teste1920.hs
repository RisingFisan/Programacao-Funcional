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