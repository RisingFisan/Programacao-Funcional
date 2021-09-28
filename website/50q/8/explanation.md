Nesta função, temos de juntar duas listas numa lista de pares, de forma a que os 1ºs, 2ºs, 3ºs, etc. elementos fiquem emparelhados. Se as listas tiverem comprimentos diferentes, a lista final fica com o comprimento da mais curta.

Como precisamos de percorrer as duas listas, iremos ter dois casos de paragem, um para cada lista.

```haskell
zip [] _ = []
zip _ [] = []
```

Assim, a função termina quando qualquer uma das listas "termine", isto é, fique vazia.

A definição recursiva da função é trivial, apenas precisamos de juntar as cabeças das listas, de modo a formar um par.

```haskell
zip (h:t) (h':t') = (h,h') : zip t t'
```

Ficamos assim com a definição final desta função:

```haskell
zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (h:t) (h':t') = (h,h') : zip t t'
```