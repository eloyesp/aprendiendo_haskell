la idea es sumar los numeros que ingresa el usuario.

para poder armar los numeros con el input del usuario parece que
tengo que hacer una funcion...

> toInt :: String -> Integer
> toInt input = read input

> main = do

preguntamos por los numeros:

>   putStrLn "ingresa un numero: "

capturamos el número:

>   input <- getLine

y lo imprimimos

>   print (toInt input)

y despues el 4... :)

>   print 4
