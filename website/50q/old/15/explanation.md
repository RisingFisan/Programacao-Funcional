Tal como na questão anterior, nesta usamos a função pré-definida `tail`, que devolve a cauda de uma lista, isto é, a lista sem o primeiro elemento, de forma recursiva.

Obtemos assim a seguinte função:

```haskell
tails :: [a] -> [[a]]
tails [] = [[]]
tails l = l : tails (tail l)
```

Como podemos ver, é praticamente igual à função `inits`. A única diferença, para além do nome das funções, é que aqui colocamos a própria lista à esquerda do resto do resultado, em vez de colocar à direita, de modo a que as sub-listas do resultado sejam progressivamente mais pequenas.