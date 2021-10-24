A função a definir aqui será fundamentalmente igual à função da questão anterior, com uma diferença: em vez de "pegar" em elementos, queremos "deitá-los fora".

A partir da definição de `take`:

```haskell
take :: Int -> [a] -> [a]
take _ [] = []
take n (h:t)
    | n <= 0 = []
    | otherwise = h : take (n - 1) t
```

Apenas temos de fazer umas pequenas alterações:

```haskell
drop :: Int -> [a] -> [a]
drop _ [] = []
drop n (h:t)
    | n <= 0 = h : t
    | otherwise = drop (n - 1) t
```

O que é que mudámos? Primeiro, quando `n <= 0`, em vez de devolver uma lista vazia, devemos devolver a própria lista. Segundo, na definição recursiva, em vez de introduzir os elementos que percorremos numa lista, fazemos o oposto, isto é, ignoramo-los.