{--
Ejercicio 1
Implementar una funcion
estanRelacionados :: Integer -> Integer -> Bool
que dados a y b e Z distintos de cero indique si a~b. Diremos que a~b si y solo si existe un 
k e Z distinto de cero tal que a^2 + abk = 0
--}

estanRelacionados :: Integer -> Integer -> Bool
estanRelacionados a b = (mod (-a) b == 0)
{--
Ejercicio 2
Implementar la funcion
prod :: Integer -> Integer
que para n e N>0 calcule la productoria (hasta 2n) (desde i=1) de (i^2 + 2i)
--}
prod :: Integer -> Integer
prod 0 = 0
prod n = (prodAux (2*n))

prodAux :: Integer -> Integer
prodAux n = ((n^2 + (2*n))*(prodAux(n-1)))

{--
Ejercicio 3
Implementar una funcion
esCapicua :: Integer -> Bool que dado neN>=0 determine si n es un numero capicua
--}
{- esCapicua :: Integer -> Bool
esCapicua n
 | (div n 10) == 1 = True
 | otherwise = calculoAux n (cantDigitos n) k

calculoAux :: Integer -> Integer -> Integer
calculoAux n m k
 | n < 10 = True
 | m < k = True
 | ((i_esimo n m) == (i_esimo n k)) = esCapicua (m-1) (n+1)
 | otherwise = False -}

{--
Ejercicio 4
Programar una funcion
zipPrimos :: [Integer] -> [Integer] -> [(Integer, Integer)] 
que dadas dos listas de numeros naturales de igual longitud, devuelve una lista de tuplas formadas
por numeros primos de ambas listas tal que
a) La primer coordenada de las tuplas proviene de numeros de la primera lista pasada como parametro y la segunda
coordenada proviene de numeross de la segunda lista
b) Solo se deben agregar tuplas a la lista resultante cuando aparecen numeros primos en la misma 
posicion en ambas listas pasadas como parametro
c) la lista resultante debe seguir el mismo orden de aparicion de los numeros primo que en las 
listas pasadas como parametro
--}
zipPrimos :: [Integer] -> [Integer] -> [(Integer, Integer)] 
zipPrimos [] _ = []
zipPrimos _ [] = []
zipPrimos l1 l2
 | length l1 == length l1 = zipAux l1 l2
 | otherwise = []

zipAux :: [Integer] -> [Integer] -> [(Integer, Integer)]
zipAux l1 l2
 | (length l1 == 1) && (length l2 == 1) && ((esPrimo (head l1)) && (esPrimo (head l2))) = ((head l1),(head l2)):[]
 | ((esPrimo (head l1)) && (esPrimo (head l2))) = ((head l1),(head l2)):(zipAux (tail l1) (tail l2))
 | otherwise = zipAux (tail l1) (tail l2)

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k 
 | n == k = n
 | mod n k == 0 = k
 | otherwise = menorDivisorDesde n (k + 1)

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

esPrimo :: Integer -> Bool
esPrimo 1 = True
esPrimo n = menorDivisor n == n

{--Ejercicio 5
Se define la sucesion a1=1, a2=2, a3=5, an+1 = 3an^2 + 2an-1+an-2 con n>3.
Programar la funcion 
promedioTerminos :: [Integer] -> Integer
que dada una lista de numeros naturales calcule el promedio de todos los terminos de la sucesion an que 
aparecen en la lista. Si la lista no contiene terminos de la sucesion la funcion debe retornar 0
Asumir que la funcion longitud :: [Integer] -> Integer devuelve la longitud de la lista
--}
{- promedioTerminos :: [Integer] -> Integer
promedioTerminos lista
 | (longitud (listaConTerminosSuc lista)) == 0 = 0
 | otherwise = ((sumaTerminos (listaConTerminosSuc lista))/fromInteger (longitud (listaConTerminosSuc lista)))


sucesion :: Integer -> Integer
sucesion 1 = 1
sucesion 2 = 2
sucesion 3 = 5
sucesion n = 3*((sucesion (n-1))^2)+(2*(sucesion (n-2))) + sucesion (n-3) -}