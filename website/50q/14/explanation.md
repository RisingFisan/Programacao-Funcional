De modo a definir a função `inits`, precisamos primeiro de conhecer a função pré-definida `init`. Esta função, a partir de uma lista, devolve o seu prefixo, isto é, retira o seu último elemento. Por exemplo, `init [1,2,3,4] = [1,2,3]`.

Sabemos que o resultado de `inits [1,2,3,4]` deverá ser `[[],[1],[1,2],[1,2,3],[1,2,3,4]]`. Ora, sabendo o que a função `init` faz, encontramos aqui um padrão. Cada sub-lista, da direita para a esquerda, é igual à sub-lista anterior, tirando o último elemento, que desaparece. Ora, que função é que retira o último elemento de uma lista? Exato, `init`!

Ficamos assim a saber que, para cada sub-lista do resultado, a sub-lista à sua esquerda terá que ser o resultado de aplicar a função `init` a si mesma, de forma recursiva.

Obtemos assim:

```haskell
inits l = inits (init l) ++ [l]
```

Agora acrescentamos o caso de paragem e ficamos com a função definida:

```haskell
inits :: [a] -> [[a]]
inits [] = [[]]
inits l = inits (init l) ++ [l]
```