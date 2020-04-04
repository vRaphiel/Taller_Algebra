{--Ejercicio 1
Programar la funcion
sumatoriaPares :: [Integer] -> Integer
que devuelve la suma de los pares enteros de la lista
--}

sumatoriaPares :: [Integer] -> Integer
sumatoriaPares [] = 0
sumatoriaPares (x:[]) 
 | esPar x = x
 | otherwise = 0
sumatoriaPares lista = sumatoriaParesAuxiliar lista 0

sumatoriaParesAuxiliar :: [Integer] -> Integer -> Integer
sumatoriaParesAuxiliar (x:xs) suma
 | length xs == 0 && esPar x = (suma + (valorAbsoluto x))
 | length xs == 0 = suma
 | esPar x = sumatoriaParesAuxiliar xs (suma + (valorAbsoluto x))
 | otherwise = sumatoriaParesAuxiliar xs suma

esPar :: Integer -> Bool
esPar 0 = False
esPar numero 
 | (mod numero 2) == 0 = True
 | otherwise = False

valorAbsoluto :: Integer -> Integer
valorAbsoluto 0 = 0
valorAbsoluto n 
 | n > 0 = n
 | n < 0 = (-1) * n

{-- Ejercicio 2
Programar la funcino filtro que quita todas las apariciones en un lista de un numero X
--}
filtro ::[Integer] -> Integer -> [Integer]
filtro [] n = []
filtro lista 0 = lista
filtro (x:xs) n
 | n == x = (filtro xs n)
 | otherwise = x:(filtro xs n)

{--
Ejercicio 3
Proramar la funcion 
posiciones :: Integer -> [Integer] -> [Integer] tal que posiciones
x ys devuelve todas las posiciones del elemento x que estan en la lista ys y devuelve [] e caso de que x no pertenezca
a la lista. Las posiciones pueden ser devueltas en cualquier orden
--}

posiciones :: Integer -> [Integer] -> [Integer]
posiciones _ [] = []
posiciones numero lista = calculoPosicionesAux numero 1 lista

calculoPosicionesAux :: Integer -> Integer -> [Integer] -> [Integer]
calculoPosicionesAux a count lista
 | length lista == 0 = []
 | a == (head lista) = count:(calculoPosicionesAux a (count+1) (tail lista))
 | otherwise = calculoPosicionesAux a (count+1) (tail lista)

pertenece :: Eq a => a -> [a] -> Bool
pertenece a b
 | length b > 0 && a == head b = True
 | length b == 0 = False
 | otherwise = pertenece a (tail b)

{--
Ejercicio 4
Programar la funcion inserta :: Integer -> [Integer] -> [Integer]
que dado un numero e y una lista l ordenada de menor a mayor, agrega informacion ordenada,
es decir, agrega el numero e delante del primer elemento que sea mayor o igual que el 
Luego, programe la funcion
ordenarPorInserccion :: [Integer] -> [Integer] que ordena una lista mediante una inserccion
--}
inserta :: Integer -> [Integer] -> [Integer]
inserta numero [] = numero:[]
inserta numero lista
 | (head lista <= numero) = (head lista):(inserta numero (tail lista))
 | otherwise = numero:lista

ordenarPorInserccion :: [Integer] -> [Integer]
ordenarPorInserccion lista 
 | length lista == 0 = []
 | otherwise =  inserta (head lista) (ordenarPorInserccion (tail lista))
