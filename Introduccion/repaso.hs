{--
Recursividad
-> Caso Base Obligatorio
-> Caso general
-> 
--}

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

sumaDivisores :: Integer -> Integer
sumaDivisores n = sumaDivisoresHasta n n

sumaDivisoresHasta :: Integer -> Integer -> Integer
sumaDivisoresHasta n 0 = 0
sumaDivisoresHasta n m 
 | n `mod` m == 0 = m + sumaDivisoresHasta n (m-1)
 | otherwise = sumaDivisoresHasta n (m-1)

 sumaInterior :: Integer -> Integer -> Integer
 sumaInterior n 0 = 0
 sumaInterior n m = (n*m) + sumaInterior n (m-1)

 sumaDoble :: Integer -> Integer -> Integer
 sumaDoble 0 m = 0
 sumaDoble n m = sumaInterior n m + sumaDoble (n-1) m

 longitud :: [Integer] -> Integer
 longitud [] = 0
 longitud (_:xs) = 1 + longitud xs

contarOvejas :: [Bool] -> Integer
contarOvejas n = (contarOvejasAux n 0)

contarOvejasAux :: [Bool] -> Integer -> Integer
contarOvejasAux lista number 
 | length lista == 0 = number
 | head lista = contarOvejasAux (tail lista) (number+1)
 | not (head lista)= contarOvejasAux (tail lista) (number)

{--
-- Lista con un elemento
f [] = 0
f (x:[]) = x // Un elemento
f (x:xs) = f xs // Mas de un elemento
-- Lista con dos elementos
g [x,y] => g(x:y:[])
--}