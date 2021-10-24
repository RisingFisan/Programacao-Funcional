A definição desta função é trivial, dado que já sabemos percorrer listas de forma recursiva.

Para uma lista `(h:t)`, se quisermos a sua inversa, a primeira coisa que temos de fazer é colocar `h` no fim da lista, ou seja, o primeiro elemento passa a ser o último. Temos assim `reverse (h:t) = ?? ++ [h]` (já definimos (++) na questão 3, por isso não vale a pena explicar aqui o que faz).

A melhor forma de descobrir o que deverá ser `??` é usar um exemplo concreto. Para a lista `[1,2,3]`, podemos fazer `reverse (1:[2,3]) = ?? ++ [1]`. Ora, sabemos que a inversa de `[1,2,3]` tem que ser `[3,2,1]`, ou, por outras palavras, `[3,2] ++ [1]`. Logo, neste caso, `?? = [3,2]`, que não é nada mais do que a inversa da cauda da lista inicial.

Desta forma, podemos definir `reverse (h:t) = reverse t ++ [h]`.

Agora, apenas nos falta o caso de paragem. Com a prática que já temos, chegamos rapidamente a `reverse [] = []`.

Temos assim a nossa função final:

```haskell
reverse :: [a] -> [a]
reverse [] = []
reverse (h:t) = reverse t ++ [h]
```