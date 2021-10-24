Para escrever esta função de forma recursiva, iremos ter que dividir uma das listas, visto que a recursividade se baseia na divisão de um problema grande em pequenas instâncias do mesmo. Nesta explicação, escolhi dividir a primeira lista. Em Haskell, é geralmente mais fácil aplicar recursividade da esquerda para a direita, principalmente em listas, já que temos uma função para inserir um elemento no início de uma lista (à esquerda), mas não para inserir no fim (à direita).

Assim, para a função `(++) a b`, sendo `a` e `b` duas listas, se dividirmos a primeira lista em `h`, o seu elemento inicial, e `t`, o resto da lista (*h* e *t* vêm de *head* e *tail*, "cabeça" e "cauda" em inglês, respetivamente), temos `(++) (h:t) b`.

Sabemos que o resultado esperado desta função é `a ++ b`, uma concatenação das duas listas (por outras palavras, `(++) a b = a ++ b`). Se substituirmos `a` no resultado pela nossa divisão, ficamos com `(h:t) ++ b`.

Por intuição, vemos que podemos remover o `h` dos parênteses, tanto faz acrescentar `h` à lista antes ou depois da concatenação, visto que a concatenação não altera o início da primeira lista, e ficamos com `h : (t ++ b)`.

Ora, `t ++ b` não é nada mais do que `(++) t b`, a nossa função aplicada às duas listas.

Desta forma, podemos deduzir que `(++) (h:t) b = h : (++) t b`, e chegamos assim à definição recursiva da função.

Agora, apenas precisamos do caso de paragem. Nesta função é fácil encontrá-lo, como estamos a encolher a primeira lista, apenas temos de ver quando é que não a podemos encolher mais. Aqui a resposta é "quando ela for vazia", e neste caso a lista final deverá ser a segunda lista, já que concatenar algo com nada dá o "algo".

Sendo assim, obtemos a nossa solução final.

```haskell
(++) :: [a] -> [a] -> [a]
(++) [] b = b
(++) (h:t) b = h : (++) t b
```

Para casos em que a segunda lista seja vazia a nossa função também funciona, pois nenhuma das operações que realizamos envolvem os elementos da lista, apenas a lista em si, se está vazia ou não é irrelevante.