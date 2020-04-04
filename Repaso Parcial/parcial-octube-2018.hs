{--
Ejercicio 1
Definir una funcion 
suma2 :: (Integer, Integer, Integer) -> Integer -> Bool
que dada una tipla (a,b,c) de numeros enteros y un entero n devuelva verdadero si dos de las coordenadas
de la tupla suman n y falso en caso contrario
--}
suma2 :: (Integer, Integer, Integer) -> Integer -> Bool
suma2 (x,y,z) w = (x+y == w) || (x+z == w) || (y+z == w)

{--
Ejercicio 2
Programar la funcion 
sumatoria :: Integer -> Integer
que dado un entero n>= 1 calcule:
S(n) = desde i = 1 hasta n de (2i - i)^2
--}
sumatoria :: Integer -> Integer
sumatoria 0 = (-1)
sumatoria 1 = 1
sumatoria n = (((2*n)-n)^2) + sumatoria (n-1)
-- o, por simplificacion
-- sumatoria n = (n^2) + sumatoria (n-1)

{--
Ejercicio 3
programar la funcion
todosIguales :: [Integer] -> Bool
que dada una lista de enteros devuelva si es verdadero si todos los elementos son iguales
y
todosDistintos :: [Integer] -> Bool
que dada una lista de enteros devuelva si es verdadero si todos los elementos son diferentes
--}
todosIguales :: [Integer] -> Bool
todosIguales [] = True
todosIguales (x:[]) = True
todosIguales (x:y:[]) 
 | x == y = True
todosIguales lista
 | length lista == 0 = True
 | (head lista) == (head (tail lista)) = todosIguales (tail lista)
 | otherwise = False

todosDistintos :: [Integer] -> Bool
todosDistintos [] = True
todosDistintos (x:[]) = True
todosDistintos (x:y:[])
 | x /= y = True
todosDistintos lista = tda (tail lista) (head lista)

{--Estas funciones son como mi vida. No tienen sentido pero cumplen su objetivo--}
tda :: [Integer] -> Integer -> Bool
tda lista num
 | length lista == 0 = True
 | (head lista) /= num = tda (tail lista) num
 | otherwise = False

tda2 :: [Integer] -> Integer -> Bool
tda2 lista num
 | length lista == 0 = True
 | tda (tail lista) num = tda2 (tail lista) (head (tail lista))
 | otherwise = False

 {--
Ejercicio 4
Programar una funcion 
sacarTodos:: [Integer] -> [Integer] -> [Integer]
que dadas dos listas de enteros, quite de la primera todas las ocurrencias de los elementos
de la segunda lista
 --}
sacarTodos:: [Integer] -> [Integer] -> [Integer]
sacarTodos [] _ = []
sacarTodos xs [] = xs
sacarTodos xs (n:ns) = sacarTodos (quitar xs n) ns

quitar :: [Integer] -> Integer -> [Integer]
quitar (x:xs) n
 | length xs == 0 && x == n = []
 | length xs == 0 = []
 | x == n = quitar xs n
 | otherwise = x:(quitar xs n)

{--
Ejercicio 5
programar una funcion 
promedioDe::[(Integer, Float)] -> Integer -> Float
que dada una lista de tuplas (Integer, Float) donde la primera coordenada representa el codigo
del alumno y la segunda la nota que obtuvo en cada examen, y un codigo del alumno, retorne el
promedio de las notas de ese alumno
Puede asumir que todo alumno tiene por lo menos una nota
--}
promedioDe::[(Integer, Float)] -> Integer -> Float
promedioDe [] _ = 0
promedioDe _ 0 = 0
promedioDe lista idalumno = auxPromedios lista idalumno 0 0

auxPromedios :: [(Integer, Float)] -> Integer -> Float -> Integer -> Float
auxPromedios lista num sumaNotas alumnoEncontrado
 | length lista == 0 = (sumaNotas/fromInteger (alumnoEncontrado))
 | (fst (head lista)) == num = auxPromedios (tail lista) num (snd (head lista)+sumaNotas) (alumnoEncontrado + 1)
 | otherwise = auxPromedios (tail lista) num (sumaNotas) (alumnoEncontrado)