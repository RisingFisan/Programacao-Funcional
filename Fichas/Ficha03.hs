module Ficha03 where

import Ficha01 as F1

-- Exercício 1

-- data Hora = H Int Int deriving Show

type Etapa = (F1.Hora,F1.Hora)
type Viagem = [Etapa]

-- a)

etapaBemConstruida :: Etapa -> Bool
etapaBemConstruida (h1,h2) = F1.horaValida' h1 && F1.horaValida' h2 && h2 `horaDepois` h1

-- b)

viagemBemConstruida :: Viagem -> Bool
viagemBemConstruida [] = True
viagemBemConstruida [e] = etapaBemConstruida e
viagemBemConstruida ((h1,h2):(h3,h4):et) = etapaBemConstruida (h1,h2) && etapaBemConstruida (h2,h3) && viagemBemConstruida ((h3,h4):et)

-- c)

partidaEChegada :: Viagem -> (F1.Hora,F1.Hora)
partidaEChegada [(h1,h2)] = (h1,h2)
partidaEChegada ((h1,_):(_,h4):et) = partidaEChegada ((h1,h4):et) 

-- d)

tempoDeViagem :: Viagem -> Hora
tempoDeViagem [(h1,h2)] = hordiff' h1 h2
tempoDeViagem ((h1,h2):et) = addmins' (hordiff' h1 h2) (hor2min' (tempoDeViagem et))

-- e)

tempoDeEspera :: Viagem -> Hora
tempoDeEspera [(h1,h2)] = H 0 0
tempoDeEspera ((h1,h2):(h3,h4):et) = addmins' (hordiff' h2 h3) (hor2min' (tempoDeEspera ((h3,h4):et)))

-- f)

tempoTotalViagem :: Viagem -> Hora
tempoTotalViagem v = addmins' (tempoDeViagem v) (hor2min' (tempoDeEspera v))

-- Exercício 2

type Poligonal = [Ponto]

-- a)

comprimento :: Poligonal -> Double
comprimento p
    | null p || length p == 1 = 0
    | otherwise = dist (head p) (head (tail p)) + comprimento (tail p)

comprimento' :: Poligonal -> Double
comprimento' = sum . uncurry (zipWith dist) . split id tail
    where split f g x = (f x, g x)

-- b)

linhaFechada :: Poligonal -> Bool
linhaFechada p = length p >= 3 && head p == last p

-- c)

triangula :: Poligonal -> [Figura]
triangula [p1,p2,p3] = [Triangulo p1 p2 p3]
triangula (p1:p2:p3:ps) = Triangulo p1 p2 p3 : triangula (p1:p3:ps)
triangula _ = []

-- d)

areaPol :: Poligonal -> Double
areaPol = sum . map area . triangula

-- e)

mover :: Poligonal -> Ponto -> Poligonal
mover = flip (:)

-- f)

zoom :: Double -> Poligonal -> Poligonal
zoom z [p1,Cartesiano a b] = [p1,Cartesiano (z*a) (z*b)]
zoom z (p1:(Cartesiano a b):pol) = p1:zoom z (p3:pol)
    where p3 = Cartesiano (z*a) (z*b)
zoom _ p = p

-- Exercício 3

data Contacto = Casa Integer | Trab Integer | Tlm Integer | Email String deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]

-- a)

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email agenda = agenda ++ [(nome, [Email email])]

-- b)

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails nome [(n,c)] = if nome == n then Just (map (\(Email e) -> e) c) else Nothing
verEmails nome ((n,c):agenda) = if nome == n then verEmails nome [(n,c)] else verEmails nome agenda

-- c)

consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs (c:cs) = case c of Casa x -> x:consTelefs cs
                              Trab x -> x:consTelefs cs
                              Tlm x -> x:consTelefs cs 
                              _ -> consTelefs cs

-- d)

casa :: Nome -> Agenda -> Maybe Integer
casa nome [(n,c:cs)] = if nome == n then case c of Casa x -> Just x
                                                   _ -> casa nome [(n,cs)] 
                                      else Nothing
casa nome ((n,c):agenda) = if nome == n then casa nome [(n,c)] else casa nome agenda

-- Exercicio 4

type Dia = Int
type Mes = Int
type Ano = Int

data Data = D Dia Mes Ano deriving Show
type TabDN = [(Nome,Data)]

-- a)

procura :: Nome -> TabDN -> Maybe Data
procura nome ((n,d):ts) = if nome == n then Just d else procura nome ts

-- b)

idade :: Data -> Nome -> TabDN -> Maybe Int
idade dat@(D dx mx ax) nome ((n,D d m a):ts) = if nome == n then if mx > m || mx == m && dx > d then Just (ax - a) else Just ((ax - a) - 1) else idade dat nome ts
                                                                                                 
-- c)

anterior :: Data -> Data -> Bool
anterior (D d m a) (D d2 m2 a2) = a < a2 || (a == a2 && (m < m2 || (m == m2 && d < d2))) 

-- d)

ordena :: TabDN -> TabDN
ordena [] = []
ordena ((n,d):ts) = insere (n,d) (ordena ts)
    where insere (n,d) [] = [(n,d)]
          insere (n,d) ((nh,dh):t) | anterior dh d = (nh,dh):insere (n,d) t
                                   | otherwise = (n,d):(nh,dh):t

-- e)

porIdade:: Data -> TabDN -> [(Nome,Int)]
porIdade _ [] = []
porIdade (D d m a) tabela = (n,idade) : porIdade (D d m a) ts
    where ((n,D dx mx ax):ts) = ordena tabela
          idade = if m > mx || mx == m && d > dx then a - ax else (a - ax) - 1

-- Exercicio 5

data Movimento = Credito Float | Debito Float deriving Show

data Extracto = Ext Float [(Data, String, Movimento)] deriving Show

-- a)

extValor :: Extracto -> Float -> [Movimento]
extValor (Ext _ []) _ = []
extValor (Ext x ((_,_,mov):ls)) valor = case mov of Credito n -> if n >= valor then mov : extValor (Ext x ls) valor else extValor (Ext x ls) valor
                                                    Debito n -> if n >= valor then mov : extValor (Ext x ls) valor else extValor (Ext x ls) valor

-- b)

filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext _ []) _ = []
filtro (Ext x ((dat,desc,mov):ls)) listaStr = if desc `elem` listaStr then (dat,mov):filtro (Ext x ls) listaStr else filtro (Ext x ls) listaStr

-- c)

creDeb :: Extracto -> (Float,Float)
creDeb (Ext _ lm) = foldl (\(c,d) (_,_,mov) -> case mov of Credito x -> (c + x, d)
                                                           Debito x -> (c, d + x)) (0,0) lm

-- d)

saldo :: Extracto -> Float
saldo (Ext x lm) = foldl (\acc (_,_,mov) -> case mov of Credito n -> acc + n
                                                        Debito n -> acc - n) x lm
