evenSum toma una lista y retorna un numero...
eso sería::

 > evenSum :: [a] -> a

evenSum de una lista vacia es 0...

  > evenSum [] = 0

pero para las listas no vacias, lo que hacemos es un filter y un fold.

Esto viene a ser el filter: filter even a

Y esto el fold: foldl (+) 0 a

Y ahora todo junto:

> evenSum = foldl (+) 0 . filter even

por otro lado, podemos hacer un test muy simple que diga si lo que venimos
haciendo tiene sentido.

> main = if (evenSum [1..6]) == 12
>        then putStrLn "bien"
>        else putStrLn "mal"
