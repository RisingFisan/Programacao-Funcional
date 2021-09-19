Esta questão é muito semelhante à [questão 4](/50q/4), mas nesta, em vez de devolver apenas o n-ésimo elemento, queremos devolver todos os elementos entre o início da lista e o n-ésimo elemento, exclusive.

Vamos pegar na função da questão 4 e mudar-lhe o nome e a ordem das variáveis:

```haskell
take :: Int -> [a] -> [a]
take 0 (h:t) = h
take n (h:t) = take (n - 1) t
```

A primeira coisa a mudar é o caso de paragem. Primeiro, quando `n = 0`, queremos devolver uma lista vazia, pois não estamos a pegar em nenhum elemento: `take 0 (h:t) = []`. Segundo, ao contrário da questão 4, aqui podemos receber uma lista vazia. Devemos, portanto, definir o caso de paragem adequado: `take n [] = []`.

Como estamos a aplicar recursividade a duas variáveis, é importante definirmos 2 casos de paragem, visto que não sabemos qual destas variáveis irá atingir o seu caso de paragem primeiro. Não foi necessário fazê-lo na questão 4 porque nessa função a lista nunca poderia ser vazia, ou seja, apenas atingíamos o caso de paragem com o inteiro.

Temos agora de mudar a definição recursiva da função. Aqui, para além de percorrer a lista, queremos introduzir os elementos que percorremos numa lista. Podemos assim definir `take n (h:t) = h : take (n - 1) t`.

Já podemos definir a nossa função final:

```haskell
take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (h:t) = h : take (n - 1) t
```

Se quisermos uma definição mais completa, podemos definir o caso de `n` ser um valor negativo. Caso contrário, ao introduzir um valor negativo, a função irá simplesmente devolver a lista inicial, que não é o comportamento pretendido.

Se `n` for negativo, não iremos pegar em nenhum elemento da lista. Portanto, ficamos com:

```haskell
take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (h:t)
    | n < 0 = []
    | otherwise = h : take (n - 1) t
```

Simplificando esta definição, obtemos:

```haskell
take :: Int -> [a] -> [a]
take _ [] = []
take n (h:t)
    | n <= 0 = []
    | otherwise = h : take (n - 1) t
```