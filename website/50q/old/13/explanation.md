A partir da função definida na [questão 3](../3), é muito fácil resolver esta questão. O nosso objetivo é unir várias sub-listas, e a função `(++)` une duas sub-listas, por isso apenas precisamos de a usar de forma recursiva ao longo da lista.

```haskell
concat :: [[a]] -> [a]
concat [] = []
concat (h:t) = h ++ concat t
```