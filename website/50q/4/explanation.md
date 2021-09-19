Nas funções anteriores, apenas tínhamos como argumentos valores inteiros ou listas. Nesta função, temos de trabalhar com variáveis dos dois tipos ao mesmo tempo.

Pelo enunciado, chegamos rapidamente ao caso de paragem. Se o valor inteiro dado (chamemos-lhe `n`) for 0, o elemento da lista a devolver é o primeiro. Desta forma, podemos definir `(!!) (h:t) 0 = h`. Não usamos a lista vazia como caso de paragem porque é impossível definir esta função para uma lista vazia.

Assim, só nos falta definir a função para quando `n /= 0`. No fundo, `n` diz-nos quantos elementos da lista é que temos de percorrer até chegarmos ao elemento pretendido. Por exemplo, se `n = 3`, vamos querer percorrer 3 elementos da lista e devolver o 4º elemento. Como já sabemos percorrer listas recursivamente, esta parte torna-se trivial.

Podemos começar a definir `(!!) (h:t) n = (!!) t ??`. Qual será o valor de `??`? Se avançarmos 1 elemento na lista, já não vamos querer devolver o elemento `n` da nova lista, visto que a nova lista é mais pequena. Em vez de `n`, agora queremos o elemento `n - 1`. Por outras palavras, se quisermos o elemento 3 da lista `[1,2,3,4,5]`, ao aplicar recursividade uma vez, ficamos com a lista `[2,3,4,5]`, e agora o elemento 3 passou a ser o elemento 2. Quando `n` chegar a 0, neste caso, teremos a lista `[4,5]` e sabemos que o elemento que queremos é a cabeça da lista, tal como definimos em cima. 

Ficamos assim com `(!!) (h:t) n = (!!) t (n - 1)` e já podemos definir a nossa função:

```haskell
(!!) :: [a] -> Int -> a
(!!) (h:_) 0 = h
(!!) (_:t) n = (!!) t (n - 1)
```

NOTA: `_` representa uma variável que não é usada; como tal, não vale a pena dar-lhe um nome.