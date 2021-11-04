Esta questão é um bocado complicada de definir, porque precisamos de verificar várias condições para vários números. Como tal, a melhor forma de a definir é recorrendo a uma função auxiliar.

O nosso objetivo aqui é percorrer todos os números entre 2 e √n e verificar se o valor dado é divisível por algum destes números. Se tal acontecer, o valor não é um número primo. Caso contrário, é.

Vamos então definir a nossa função principal:

```haskell
isPrime :: Int -> Bool
isPrime n
    | n >= 2 = primeCheck n 2
    | otherwise = False
```

Aqui, `primeCheck` será a nossa função auxiliar, em que `n` é o nosso valor de *input* e `m` é o acumulador, isto é, o valor que vai começar em 2 e acabar em √n. Aqui, damos a `m` o valor 2 como valor inicial.

Sendo assim, a nossa função auxiliar irá percorrer os valores inteiros entre 2 e √n. Podemos definir algo deste género:

```haskell
primeCheck :: Int -> Int -> Bool
primeCheck n m
    | m > √n = ???
    | otherwise = ???
```

Como estamos a seguir uma ordem crescente, o caso de paragem ocorrerá quando `m` atingir um valor superior a √n.

Em Haskell, os valores reais (como raizes quadradas) são diferentes dos valores inteiros, por isso, se estivermos a trabalhar com variáveis destes dois tipos, precisamos de as converter para o mesmo tipo. Para evitar estes problemas, podemos simplificar `m > √n` para `m * m > n`, elevando os dois lados da inequação ao quadrado.

A nossa condição para um número não ser primo é ser divisível por `m`. Em Haskell, isso traduz-se em `mod n m == 0`, ou seja, a divisão de `n` por `m` dá resto (mod) nulo. Podemos então acrescentar essa condição à nossa função:

```haskell
primeCheck :: Int -> Int -> Bool
primeCheck n m
    | m * m > n = ???
    | mod n m == 0 = False
    | otherwise = ???
```

Assim, se a condição falhar para qualquer valor, sabemos que `n` não é primo. Sendo assim, também sabemos que, se atingirmos o caso de paragem, significa que `n` não é divisível por nenhum valor entre 2 e √n, logo será primo.

```haskell
primeCheck :: Int -> Int -> Bool
primeCheck n m
    | m * m > n = True
    | mod n m == 0 = False
    | otherwise = ???
```

No `otherwise`, apenas nos falta colocar a "recursão", isto é, se as condições de cima falharem, devemos verificar para o valor de `m` seguinte.

```haskell
primeCheck :: Int -> Int -> Bool
primeCheck n m
    | m * m > n = True
    | mod n m == 0 = False
    | otherwise = primeCheck n (m + 1)
```