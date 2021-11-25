De uma forma simplificada, para saber se um elemento ocorre numa lista, ou é igual à cabeça da lista, ou ocorre algures na sua cauda. Se a lista for vazia, o elemento não pode pertencer à mesma.

Definimos assim esta função da seguinte forma:

```haskell
elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (h:t) = x == h || elem x t
```

NOTA: O símbolo `||` significa "ou". Neste caso, diz-nos que a função deve retornar `True` se `x == h` ou se `elem x t`. Caso nenhuma destas expressões seja verdadeira, a função retorna `False`.

NOTA 2: Como precisamos de verificar se os elementos da lista são iguais a `x`, é importante especificar que os elementos da lista devem ser de um tipo cuja igualdade dos valores possamos verificar, daí o `Eq a` na definição do tipo da função.