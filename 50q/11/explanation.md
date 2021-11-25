Esta questão é uma das mais complicadas das 50.

Até agora, nas nossas definições recursivas, apenas temos chamado a própria função com argumentos diferentes. Contudo, nós podemos fazer mais do que isto.

Vou passar a exemplificar. Se chamarmos esta função com a lista `[1,1,2,2,3,3]` (o resultado esperado será `[[1,1],[2,2],[3,3]]`), sabemos que teremos de colocar o primeiro `1` numa sub-lista com o segundo `1`. Porém, da forma como temos definido as nossas funções, tal é impossível.

Passamos então a usar o `where`:

```haskell
group (h:t) = ???
    where r = group t
```

Assim, podemos manipular o resultado de chamar a função para a cauda da lista. Sabemos que, para a lista mencionada acima, teremos:

```haskell
group (1:[1,2,2,3,3]) = ???
    where r = group [1,2,2,3,3] -- [[1],[2,2],[3,3]]
```

Através deste método, definir a função torna-se trivial. Se a cabeça da lista for igual aos elementos da primeira sub-lista do resultado, juntamo-la a essa sub-lista. Caso contrário, criamos uma nova sub-lista apenas com a cabeça da lista.

Convertendo esta lógica para Haskell, ficamos com:

```haskell
group (h:t)
    | elem h (head r) = (h : (head r)) : tail r
    | otherwise = [h] : r
    where r = group t
```

Voltando ao exemplo acima, para `group (1:[1,2,2,3,3])`, como `1` existe na primeira sub-lista do resultado (`[[1],[2,2],[3,3]]`), a função irá retornar `1:[1] : [[2,2],[3,3]] = [[1,1],[2,2],[3,3]]`. Por outro lado, se tivermos `group [1,2,3] = group (1:[2,3])`, `r` será igual a `[[2],[3]]`. Aqui, como `1` não existe na primeira sub-lista do resultado (que é `[2]`), ficamos com `[1] : [[2],[3]] = [[1],[2],[3]]`.

Como estamos a assumir que a lista tem pelo menos dois elementos (`h` e `head r`), o nosso caso de paragem terá de incidir no caso em que a lista só tem um elemento. Assim, ficamos com:

```haskell
group :: Eq a => [a] -> [[a]]
group [] = []
group [x] = [[x]]
group (h:t)
    | elem h (head r) = (h : (head r)) : tail r
    | otherwise = [h] : r
    where r = group t
```

Uma forma alternativa de definir esta função seria com uma função auxiliar.

```haskell
group :: Eq a => [a] -> [[a]]
group [] = []
group (h:t) = insere h (group t)

insere :: Eq a => a -> [[a]] -> [[a]]
insere x [] = [[x]]
insere x (h:t)
    | elem x h = (x : h) : t
    | otherwise = [x] : (h : t)
```

Esta definição é fundamentalmente igual à primeira, mas não usa o `where`. Aqui, manipulamos o resultado da chamada recursiva na função auxiliar, em vez de o fazer na própria função. Esta versão poderá ser mais simples de entender para algumas pessoas.

Incluo ainda uma versão com funções de ordem superior, a mais simples das três, mas que poderá implicar um certo grau de conforto com este tipo de funções para entender.

```haskell
group :: Eq a => [a] -> [[a]]
group [] = []
group (h:t) = (h:takeWhile (== h) t) : group (dropWhile (== h) t)
```

Esta última versão pode ser ainda mais simplificada se usarmos a função `span`, que junta as funções `takeWhile` e `dropWhile` numa só:

```haskell
group :: Eq a => [a] -> [[a]]
group [] = []
group (h:t) = (h:a) : group b
    where (a,b) = span (== h) t
```