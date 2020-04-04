{--
Ejercicio 1
Un año es bisiesto si es multiplo de 4, salvo que sea multiplo de 100, caso que no es bisiesto,
salvo si es multiplo de 400, caso en que si es bisiesto --}
bisiesto :: Integer -> Bool
bisiesto n 
 | ((mod n 400) == 0) || ((mod n 4) == 0) = True
 | otherwise = False

 {--
 Ejercicio 2
 Dados naturales n y k copn k <= n, el numero de Stirling de segunda especie (n k) es la cantidad de formas
 de separar un conjunto de n elementos en k subconjuntos no vacios. Se puede probar que (n 1) = (n n) = 1
 y que (n+1 k) = k(n k) + (n k-1)
 Programar una funcion 
 stirling :: Integer -> Integer -> Integer
 que dados n k devuelva (n j)
 --}
stirling :: Integer -> Integer -> Integer
stirling n k 
 | (n == k) || (k == 1) = 1
 | otherwise = k*(stirling (n-1) k) + (stirling (n-1) (k-1))


{--
Ejercicio 3
Dada la funcion a : N -> N, a(n) = 
    { 
        n + 1 si n es impar
        n/2 es par
    }
programar
composiciones :: Integer -> Integer
que dado un natural n cuenta cuuantas veces hay que aplicar a para llegar a 1
--}
composiciones :: Integer -> Integer
composiciones 0 = 0
composiciones 1 = 0
composiciones n 
 | n < 0 = (-1)
 | otherwise = composicionesAux n 0

composicionesAux :: Integer -> Integer -> Integer
composicionesAux num count
 | num == 1 = count
 | (mod num 2) == 0 = composicionesAux (div num 2) (count + 1)
 | otherwise = composicionesAux (num + 1) (count + 1)

{-- 
Ejercicio 4
Programar una funcion
lugarCuadrado :: [Integer] -> Integer
que dada una lista cuente cuantos elementos coinciden con el cuadrado de su posicion
--}
lugarCuadrado :: [Integer] -> Integer
lugarCuadrado [] = 0
lugarCuadrado (1:[]) = 1
lugarCuadrado list = lugarCuadradoAux list 1 0

lugarCuadradoAux :: [Integer] -> Integer -> Integer -> Integer
lugarCuadradoAux lista count result
 | length lista == 0 = result
 | head lista == 0 = lugarCuadradoAux (tail lista) (count + 1) (result )
 | mod (count^2) (head lista) == 0 = lugarCuadradoAux (tail lista) (count + 1) (result + 1)
 | otherwise = lugarCuadradoAux (tail lista) (count+1) (result)

{--Ejercicio 5
Contiene cosas no vistas en clases
Salud2--}