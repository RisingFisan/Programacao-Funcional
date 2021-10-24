Para esta função, apenas precisamos de percorrer a lista fornecida, colocando o elemento dado em cada chamada recursiva.

```haskell
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse x (h:t) = h : x : intersperse x t
```

Contudo, esta definição tem um problema. Vamos usá-la num exemplo concreto. Qual será o resultado de `intersperse 1 [10,20,30]` com a nossa definição?

```haskell
intersperse 1 [10,20,30] =
    = 10 : 1 : intersperse 1 [20,30] =
    = 10 : 1 : 20 : 1 : intersperse 1 [30] =
    = 10 : 1 : 20 : 1 : 30 : 1 : intersperse 1 [] =
    = [10,1,20,1,30,1]
```

Temos ali um `1` a mais no fim da lista. Quando a lista só tem um elemento, a função tenta intercalar o elemento dado na mesma, colocando-o entre o elemento da lista e o fim da mesma.

Desta forma, precisamos de um novo caso de paragem.

```haskell
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [h] = [h]
intersperse x (h:t) = h : x : intersperse x t
```

Apesar da definição para a lista vazia não ser necessária para a recursão, devemos mantê-la de forma a ter uma definição mais completa. Caso contrário, ao chamar a função com uma lista vazia, obteríamos um erro.