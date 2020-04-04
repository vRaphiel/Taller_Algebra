import SolucionTP

{-- Ejercicio 1 --}
{--Dado un tablero, devuelve el mayor elemento (comparando el mayor de cada fila)--}
maximoFila :: Fila -> Integer
maximoFila (x:[]) = x
maximoFila (x:xs) = max x (maximoFila xs)

{-- Ejercicio 2 --}
masRepetido :: Tablero -> Integer
masRepetido t = masRepetidoLista (aplanar t)

{-- Ejercicio 3 --}
valoresDeCamino :: Tablero -> Camino -> [Integer]
valoresDeCamino t [] = []
valoresDeCamino t (x:xs) = (valor t x):(valoresDeCamino t xs)

{-- Ejercicio 4 --}
caminoDeCollatz :: Tablero -> Camino -> Integer -> Bool
caminoDeCollatz tablero camino numero = solCDC (valoresDeCamino tablero camino) (sucesionDeCollatz (numero))

 {-- Ejercicio 5 --}
mayorSecuenciaDeCollatz :: Tablero -> Integer -> [Integer]
mayorSecuenciaDeCollatz tablero numero = mSDCMain tablero numero

{-- Ejercicio 6 --}
mayorSecuenciaDeCollatzPermutando :: Tablero -> Integer -> [Integer]
mayorSecuenciaDeCollatzPermutando tablero numero = mSDCPMain tablero numero

{-- Funciones Auxiliares EJ 1 --}
{-- Dada una fila (lista), devuelve su mayor elemento --}
maximo :: Tablero -> Integer
maximo (fila:[]) = maximoFila fila
maximo (fila:filas) = max (maximoFila fila) (maximo filas)

{--Funciones Auxiliares EJ 2 --}
{-Devuelve el elemento mas repetido de una lista-}
masRepetidoLista :: [Integer] -> Integer
masRepetidoLista xs = masApariciones (comprimir (tuplar xs))

{-Una vez que comprimimos a [(2,1),(3,1),(2,1)] como [(2,2),(3,1)], falta ver cual tiene el snd mas grande (el snd hace referencia
a la cantidad de apariciones)
Ejemplo: masApariciones [(2,2),(3,1)] ~ como el 2 es mayor que el 1, entra por la primera guarda ~ masApariciones (2,2):[]
~ masApariciones [(2,2)] ~ 2-}
masApariciones :: [(Integer, Integer)] -> Integer
masApariciones [(x1, x2)] = x1
masApariciones ((x1, x2):(y1, y2):xs)
 | x2 >= y2 = masApariciones ((x1, x2):xs)
 | otherwise = masApariciones ((y1, y2):xs)

{-Dada una lista de tuplas de la forma (numero, 1), sumamos los 1's de todas las tuplas. Primero sumamos los 1's de la primera tupla y
lo concatenamos con la lista que resulta de volver a llamar a la funcion, pero quitando todas las apariciones de la tupla que ya
comprimimos (que fue la primera)
Ejemplo: comprimir [(2,1),(3,1),(2,1)] ~ (comprimirPrimero [(2,1),(3,1),(2,1)]) ++ (comprimir (quitarTupla (2,1) [(2,1),(3,1),(2,1)]))
~ [(2,2)] ++ (comprimir [(3,1)]) ~ [(2,2)] ++ (comprimirPrimero [(3,1)]) ++ (comprimir (quitarTupla (3,1) [(3,1)]))
~ [(2,2)] ++ [(3,1)] ++ comprimir [] ~ [(2,2),(3,1)] ++ [] ~ [(2,2),(3,1)]-}
comprimir :: [(Integer, Integer)] -> [(Integer, Integer)]
comprimir [] = []
comprimir (x:xs) = (comprimirPrimero (x:xs))++(comprimir (quitarTupla x (x:xs)))

{-En esta parte sumamos los 1's de la primer tupla
Ejemplo: comprimirPrimero [(2,1),(3,1),(2,1)] ~ como 2 y 3 son distinos entra por la segunda guarda ~ 
comprimirPrimero [(2,1),(2,1)] ~ como 2 y 2 son iguales entra por la primera guarda ~ comprimirPrimero (2,1+1):[] ~ [(2,2)]-}
comprimirPrimero :: [(Integer, Integer)] -> [(Integer, Integer)]
comprimirPrimero [x] = [x]
comprimirPrimero ((x1, y1):(x2,y2):xs) | x1 == x2 = comprimirPrimero ((x1, y1 + y2):xs)
                                       | otherwise = comprimirPrimero ((x1, y1):xs)   

{-- Convierte un tablero en una lista (concatenando las filas) --}
aplanar :: Tablero -> [Integer]
aplanar [] = []
aplanar t = (head t) ++ (aplanar (tail t))

{-Dada una lista, crea una lista de tuplas donde cada fst de cada tupla es un elemento de la lista y el snd siempre es un 1. El 1
hace referencia a la cantidad de apariciones de numeros iguales (mas adelante, en otra funcion veremos que tuplas tienen el mismo 
fst y a esas les sumaremos esos 1's). Ejemplo: tuplar [2,3,2] devuelve [(2,1),(3,1),(2,1)]-}
tuplar :: [Integer] -> [(Integer, Integer)]
tuplar [] = []
tuplar (x:xs) = (x, 1):(tuplar xs)

{-- Dada una tupla y una lista de tuplas, quita todas las apariciones de la tupla de la lista --}
quitarTupla :: (Integer, Integer) -> [(Integer, Integer)] -> [(Integer, Integer)]
quitarTupla _ [] = []
quitarTupla n (x:xs)
 | n == x = quitarTupla n xs
 | otherwise = x:(quitarTupla n xs)

{-- Ejercicio 3 --}
{-Dado un tablero y un camino, busca el valor de la primera posicion del camino (el primer elemento) y vuelve a llamar a la funcion
con las demas posiciones del camino (los demas elementos = la tail de camino). Cuando ya no hay mas elementos en el camino, devuelve
la lista vacía, que une todo en una lista-}


{-- Funcion auxiliar ejercicio 4 --}
{-- Verifico si los valores del camino corresponden a la sucesion de collatz --}
solCDC :: [Integer] -> [Integer] -> Bool
solCDC (x:[]) (y:camino) = (x==y)
solCDC (x:caminoColl) (y:camino)
 | x == y = solCDC caminoColl camino
 | otherwise = False


{-- Funcion auxiliar Ejercicio 5 --}
{-- recorro el tablero en busqueda de sucesiones de collatz, generando caminos --}
mayorSDCAux :: Tablero -> Integer -> [Integer] -> [Posicion] -> Conjunto [Integer]
mayorSDCAux tablero numero sucColl listaPosiciones 
 | length listaPosiciones == 0 = []
 | pertenece numero sucColl = (rteBC tablero (head listaPosiciones) (head listaPosiciones) sucColl False):(mayorSDCAux tablero numero sucColl (tail listaPosiciones))
 | otherwise = [[numero]]

{-- Reducir conjunto de caminos de Collatz --}
reduxConjunto :: Conjunto [Integer] -> [Integer] -> [Integer]
reduxConjunto conjunto lista 
 | length conjunto == 0 = lista
 | length (head conjunto) >= length lista = reduxConjunto (tail conjunto) (head conjunto)
 | otherwise = reduxConjunto (tail conjunto) lista
-- Example : [[32,16,8,4,2]]

-- rteBC = recorrerTableroEnBuscaCamino
{-- Busco en el tablero el camino a partir del numero y comparo si el valor pertenece a la sucesion de Collatz,
si pertenece, me fijo si es el elemento siguiente al que le corresponde en la sucesion, si es asi, lo devuelvo, si no,
devuelvo lo que he obtenido hasta la posicion X--}
rteBC :: Tablero -> Posicion -> Posicion -> [Integer] -> Bool -> [Integer]
rteBC tablero pos prevPos sucColl lastChance 
 | posValida tablero pos == False && lastChance == False = rteBC tablero ((fst prevPos)+1, snd prevPos) prevPos (sucColl) True
 | posValida tablero pos == False && lastChance == True = []
 | isValid tablero pos sucColl = (valor tablero pos):(rteBC tablero (fst pos, (snd pos) + 1) pos (tail sucColl)) False
 | isValid tablero ((fst prevPos) + 1, snd prevPos) sucColl = rteBC tablero ((fst prevPos)+1, snd prevPos) prevPos (sucColl) False
 | otherwise = []
 where 
    isValid tab pos sucColl
        | posValida tablero pos == False = False
        | pertenece (valor tablero pos) sucColl && ((valor tablero pos) == (head sucColl)) = True
        | otherwise = False

{-- Funciones Auxiliares ejercicio 6 --}
{--Funcion principal de mayorSecuenciaDeCollatzPermutando
La escribo para no tener algo enorme en la funcion principal, estetica mas que nada
Primero busco todas las permutaciones de un tablero, despues de obtenerlas, busco los caminos de
collatz de cada permutacion en base a un numero, filtro por los caminos que tengan datos,
ordeno la lista por cantidad de elementos y voy quitando todas hasta quedarme con el ultimo camino el cual,
es el mas largo en base a una lista ordenada
--}
mSDCPMain :: Tablero -> Integer -> [Integer]
mSDCPMain tablero numero = filtrarElMayor 
  (ordenarPorInserccionLista 
    (filtrarSDC 
      (conjuntoCollatz 
        (permutaciones tablero) 
      numero)
    )
  )

{-- Filtro por el mayor camino de collatz --}
filtrarElMayor :: Conjunto [Integer] -> [Integer]
filtrarElMayor [] = []
filtrarElMayor (x:[]) = x
filtrarElMayor conjunto = filtrarElMayor (tail conjunto) 

{-- Filtro que caminos estan vacios --}
filtrarSDC :: Conjunto [Integer] -> Conjunto [Integer]
filtrarSDC conjunto 
 | length conjunto == 0 = []
 | length (head conjunto) /= 0 = (head conjunto):(filtrarSDC (tail conjunto))
 | otherwise = filtrarSDC (tail conjunto)

{-- Funcion que ejecuta lo principal de la function --}
mSDCMain :: Tablero -> Integer -> [Integer]
mSDCMain tablero numero 
 | numero == 1 = [1]
 | perteneceAlTab tablero numero = reduxConjunto (mayorSDCAux tablero numero (sucesionDeCollatz numero) (obtenerPosNum tablero numero)) []
 | otherwise = []

{-- Obtengo todos los caminos de Collatz de todas las permutaciones--}
conjuntoCollatz :: Conjunto Tablero -> Integer -> Conjunto [Integer]
conjuntoCollatz conjunto numero
 | length conjunto == 0 =[]
 | otherwise = (mSDCMain (head conjunto) numero):(conjuntoCollatz (tail conjunto) numero)

{-- Funciones de Permutacion --}
vacio :: Conjunto a
vacio = []

perteneceP :: Tablero -> Conjunto Tablero -> Bool
perteneceP _ [] = False
perteneceP x (y:cy) = x == y || (perteneceP x cy)

agregar :: Tablero -> Conjunto Tablero -> Conjunto Tablero
agregar tab ctab
 | perteneceP tab ctab = ctab
 | otherwise = tab:ctab

union :: Conjunto Tablero -> Conjunto Tablero -> Conjunto Tablero
union [] cy = cy
union (x:cx) cy = agregar x (union cx cy)

long :: [a] -> Integer
long [] = 0
long (_:xs) = 1 + long xs

permutaciones :: Tablero -> Conjunto Tablero
permutaciones (x:[]) = agregar [x] vacio
permutaciones n = insertarEnTodaListaEnTodaPos (head n) (permutaciones (tail n))

insertarEnTodaListaEnTodaPos :: Fila -> Conjunto Tablero -> Conjunto Tablero
insertarEnTodaListaEnTodaPos n (xs:[]) = insertarEnTodaPos n xs
insertarEnTodaListaEnTodaPos n (xs:cxs) = union (insertarEnTodaPos n xs) (insertarEnTodaListaEnTodaPos n cxs)

insertarEnTodaPos :: Fila -> Tablero -> Conjunto Tablero
insertarEnTodaPos fila tablero = insertarEnTodaPosHasta fila tablero (long tablero + 1)

insertarEnTodaPosHasta :: Fila -> Tablero -> Integer -> Conjunto Tablero
insertarEnTodaPosHasta fila tablero 1 = agregar (fila:tablero) vacio
insertarEnTodaPosHasta fila tablero k =  agregar (insertarEnPos fila tablero k) (insertarEnTodaPosHasta fila tablero (k-1))

insertarEnPos :: Fila -> Tablero -> Integer -> Tablero
insertarEnPos fila tab 1 = fila:(tab) -- Ya son listas
insertarEnPos fila (x:tab) posicion = x:(insertarEnPos fila tab (posicion-1))

{--Auxiliares Generales--}
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece a (x:xs) = a == x || pertenece a xs

collatz :: Integer -> Integer
collatz n 
 | mod n 2 == 0 = div n 2
 | otherwise = 3 * n + 1

sucesionDeCollatz :: Integer -> [Integer]
sucesionDeCollatz n
 | n == 1    = 1:[]
 | otherwise = n : sucesionDeCollatz (collatz n)

inserta :: (Integer, Integer) -> Conjunto (Integer, Integer) -> Conjunto (Integer, Integer)
inserta tupla [] = tupla:[]
inserta tupla lista
 | (fst (head lista) < fst (tupla)) = (head lista):(inserta tupla (tail lista))
 | (fst (head lista) <= fst (tupla)) && (snd (head lista) <= snd (tupla)) = (head lista):(inserta tupla (tail lista))
 | otherwise = tupla:lista

ordenarPorInserccion :: Conjunto (Integer, Integer) -> Conjunto (Integer, Integer)
ordenarPorInserccion lista 
 | length lista == 0 = []
 | otherwise =  inserta (head lista) (ordenarPorInserccion (tail lista))

eliminarRepetidos :: Conjunto (Integer, Integer) -> Conjunto (Integer, Integer)
eliminarRepetidos [] = []
eliminarRepetidos [x] = [x]
eliminarRepetidos (x1:x2:xs) 
 | x1 == x2 = eliminarRepetidos(x2:xs) 
 | otherwise = x1:eliminarRepetidos(x2:xs)

perteneceAlTab :: Tablero -> Integer -> Bool
perteneceAlTab tab num = recorrerCol tab num 1 (cantidadFilas tab) (cantidadColumnas tab)
 
-- recorrerCol sopan x pos_y (cantidadFilas sopan) (cantidadColumnas sopan)
recorrerCol :: Tablero -> Integer -> Integer -> Integer -> Integer -> Bool
recorrerCol tablero numero pos_Y max_Y max_X
 | pos_Y > max_Y = False
 | pos_Y > max_Y = recorrerCol tablero numero (pos_Y + 1) max_Y max_X
 | pos_Y <= max_Y && (recorrerF tablero numero 1 pos_Y max_X) = True
 | otherwise = recorrerCol tablero numero (pos_Y + 1) max_Y max_X

-- recorrerF sopan x pos_X pos_Y (cantidadColumnas sopan)
recorrerF :: Tablero -> Integer -> Integer -> Integer -> Integer -> Bool
recorrerF tablero num pos_X pos_Y max_x
 | (posValida tablero (pos_Y, pos_X) == False) = False
 | pos_X > max_x = False
 | valor tablero (pos_Y, pos_X) == num = True
 | otherwise = recorrerF tablero num (pos_X + 1) pos_Y max_x

recorrerFoi :: Tablero -> Integer -> Integer -> Integer -> Integer -> Posicion
recorrerFoi tablero num pos_X pos_Y max_x
 | pos_X > max_x = (1, 1)
 | valor tablero (pos_Y, pos_X) == num = (pos_Y, pos_X)
 | otherwise = recorrerFoi tablero num (pos_X + 1) pos_Y max_x

{-- Obtenemos todas las posiciones de los elementos --}
obtenerPosNum :: Tablero -> Integer -> [Posicion]
obtenerPosNum tablero numero = eliminarRepetidos (ordenarPorInserccion (aplanarND (deconstruir (recorrerAux tablero numero))))

aplanarND :: [[Posicion]] -> [Posicion]
aplanarND [] = []
aplanarND t = (head t) ++ (aplanarND (tail t))
 
deconstruir :: [[Posicion]] -> [[Posicion]]
deconstruir n 
 | length n == 0 = []
 | length (head n) == 0 = deconstruir (tail n)
 | otherwise = (head n):(deconstruir(tail n))

recorrerAux :: Tablero -> Integer -> [[Posicion]]
recorrerAux tablero numero = recorrerColPos tablero numero 1 1 (cantidadFilas tablero) (cantidadColumnas tablero)

recorrerColPos :: Tablero -> Integer -> Integer -> Integer -> Integer -> Integer -> [[Posicion]]
recorrerColPos tablero numero pos_Y pos_X max_Y max_X
 | pos_X > max_X = []
 | pos_Y > max_Y = recorrerColPos tablero numero 1 (pos_X + 1) max_Y max_X
 | pos_Y <= max_Y = (obtenerPos tablero numero 1 pos_Y max_X):(recorrerColPos tablero numero (pos_Y + 1) pos_X max_Y max_X)
 | otherwise = recorrerColPos tablero numero (pos_Y + 1) pos_X max_Y max_X

obtenerPos :: Tablero -> Integer -> Integer -> Integer -> Integer -> [Posicion]
obtenerPos tablero num pos_X pos_Y max_x
 | pos_X > max_x = []
 | valor tablero (pos_Y, pos_X) == num = (pos_Y, pos_X):(obtenerPos tablero num (pos_X + 1) pos_Y max_x)
 | otherwise = obtenerPos tablero num (pos_X + 1) pos_Y max_x

insertaLista :: [Integer] -> Conjunto [Integer] -> Conjunto [Integer]
insertaLista lista [] = lista:[]
insertaLista lista conjunto
 | length (head conjunto) < length (lista) = (head conjunto):(insertaLista lista (tail conjunto))
 | otherwise = lista:conjunto

ordenarPorInserccionLista :: Conjunto [Integer] -> Conjunto [Integer]
ordenarPorInserccionLista lista 
 | length lista == 0 = []
 | otherwise =  insertaLista (head lista) (ordenarPorInserccionLista (tail lista))