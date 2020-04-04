import SolucionTP
import Motion

{-- Ejercicio 1 --}
maximo :: Tablero -> Integer
maximo tablero = maxNumPorTab tablero 0

{-- Funciones Auxiliares EJ 1 --}
{-- Recorro las filas del tablero --}
maxNumPorTab :: Tablero -> Integer -> Integer
maxNumPorTab (x:[]) numero = maxNumPorFila x numero
maxNumPorTab (x:tabX) maxNum 
 | (maxNumPorFila x 0) >= maxNum = maxNumPorTab tabX (maxNumPorFila x 0)
 | otherwise = maxNumPorTab tabX maxNum

{-- Recorro las columnas del tablero--}
maxNumPorFila :: Fila -> Integer -> Integer
maxNumPorFila (x:[]) numero 
 | x > numero = x
 | otherwise = numero
maxNumPorFila (x:rest) numero 
 | x >= numero = maxNumPorFila rest x
 | otherwise = maxNumPorFila rest numero

{-- Ejercicio 2 --}
masRepetido :: Tablero -> Integer
masRepetido tablero = fst (devolverTupla (manipularConjuntoRep tablero) (0,0))

{--Funciones Auxiliares EJ 2 --}
{--Genero una tupla con el valor mas repetido y la cantidad de veces que aparece--}
devolverTupla :: Conjunto (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
devolverTupla conjunto tupla
 | length conjunto == 0 = tupla
 | snd (head conjunto) >= snd tupla = devolverTupla (tail conjunto) (head conjunto)
 | otherwise = devolverTupla (tail conjunto) tupla

{--Ordeno el conjunto de tuplas obtenido y elimino los repetidos para quedarme con un conjunto equivalente--}
manipularConjuntoRep :: Tablero -> Conjunto (Integer,Integer)
manipularConjuntoRep tab = eliminarRepetidos (ordenarPorInserccion (callAct tab))

{--Llamo a armarConjuntoTuplas--}
callAct :: Tablero -> Conjunto (Integer,Integer)
callAct tab = armarConjuntoTuplas tab 1 1 (cantidadFilas tab) (cantidadColumnas tab)

{-- armarConjuntoTuplas: Armar Conjunto Tuplas
Recorro el tablero en pos_x y pos_y para obtener los valores
--}
armarConjuntoTuplas :: Tablero -> Integer -> Integer -> Integer -> Integer -> Conjunto (Integer, Integer)
armarConjuntoTuplas tablero index_x index_y max_x max_y
 | index_x > max_x = []
 | index_y > max_y = armarConjuntoTuplas tablero ( index_x + 1 ) 1 max_x max_y
 | otherwise = ( buildArmarTupla tablero ( valor tablero ( index_x , index_y ) ) ) : ( armarConjuntoTuplas tablero index_x ( index_y + 1 ) max_x max_y )

--armarConjuntoTuplas sopa1 1 1 4 5
-- IT WORKS ON armarConjuntoTuplas sopa1 1 1 4 4 

-- vCECT: Valor contenido en conjunto de tuplas
-- ca : Conjunto auxiliar
{-- Reviso si el valor existe en el conjunto
Funcion a revisar si es importante o no --}
vCECT :: Integer -> Conjunto (Integer, Integer) -> Bool
vCECT n [] = False
vCECT n ca
 | n == fst (head ca) = True
 | otherwise = vCECT n (tail ca)

{-- Llamo a armar tupla --}
buildArmarTupla :: Tablero -> Integer -> (Integer, Integer)
buildArmarTupla tablero numero = armarTupla tablero numero 0 1 (cantidadFilas tablero) (cantidadColumnas tablero)

{-- Armo la tupla --}
armarTupla :: Tablero -> Integer -> Integer -> Integer -> Integer -> Integer -> (Integer, Integer)
armarTupla tablero numero cantidad y posMY posMX
 | y > posMY = (-1,-1)
 | y == posMY = (numero, cantidad + (recorrerFila tablero numero 1 posMY 0 posMX))
 | otherwise = armarTupla tablero numero (cantidad + (recorrerFila tablero numero 1 y 0 posMX)) (y+1) posMY posMX

{-- Recorro la fila para obtener el valor --}
recorrerFila :: Tablero -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer
recorrerFila tablero num posNX posY count max_x
 | posNX > max_x = count
 | valor tablero (posY, posNX) == num = recorrerFila tablero num (posNX + 1) posY (count+1) max_x
 | otherwise = recorrerFila tablero num (posNX + 1) posY count max_x

{-- Ejercicio 3 --}
valoresDeCamino :: Tablero -> Camino -> [Integer]
valoresDeCamino tablero camino
 | length camino == 0 = []
 | otherwise = (valor tablero (head camino)):(valoresDeCamino tablero (tail camino))

{-- Ejercicio 4 --}
caminoDeCollatz :: Tablero -> Camino -> Integer -> Bool
caminoDeCollatz tablero camino numero = solCDC (valoresDeCamino tablero camino) (sucesionDeCollatz (numero))

{-- Funcion auxiliar ejercicio 4 --}
{-- Verifico si los valores del camino corresponden a la sucesion de collatz --}
solCDC :: [Integer] -> [Integer] -> Bool
solCDC (x:[]) (y:camino) = (x==y)
solCDC (x:caminoColl) (y:camino)
 | x == y = solCDC caminoColl camino
 | otherwise = False

{-- Ejercicio 5 --}
mayorSecuenciaDeCollatz :: Tablero -> Integer -> [Integer]
mayorSecuenciaDeCollatz tablero numero 
 | numero == 1 = [1]
 | perteneceAlTab tablero numero = reduxConjunto (mayorSDCAux tablero numero (sucesionDeCollatz numero) (obtenerPosNum tablero numero)) []
 | otherwise = []

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

{-- Ejercicio 6 --}

mayorSecuenciaDeCollatzPermutando :: Tablero -> Integer -> [Integer]
mayorSecuenciaDeCollatzPermutando tablero numero = mSDCPMain tablero numero

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

{-- Obtengo todos los caminos de Collatz de todas las permutaciones--}
conjuntoCollatz :: Conjunto Tablero -> Integer -> Conjunto [Integer]
conjuntoCollatz conjunto numero
 | length conjunto == 0 =[]
 | otherwise = (mayorSecuenciaDeCollatz (head conjunto) numero):(conjuntoCollatz (tail conjunto) numero)

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

{--Funciones Auxiliares--}
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
 | (posValida tablero (pos_X, pos_Y) == False) = False
 | pos_X > max_x = False
 | valor tablero (pos_Y, pos_X) == num = True
 | otherwise = recorrerF tablero num (pos_X + 1) pos_Y max_x

recorrerFoi :: Tablero -> Integer -> Integer -> Integer -> Integer -> Posicion
recorrerFoi tablero num pos_X pos_Y max_x
 | (posValida tablero (pos_Y, pos_X) == False) = (pos_Y, pos_X)
 | pos_X > max_x = (1, 1)
 | valor tablero (pos_Y, pos_X) == num = (9, 9)
 | otherwise = recorrerFoi tablero num (pos_X + 1) pos_Y max_x

{-- Obtenemos todas las posiciones de los elementos --}
obtenerPosNum :: Tablero -> Integer -> [Posicion]
obtenerPosNum tablero numero = eliminarRepetidos (ordenarPorInserccion (aplanar (deconstruir (recorrerAux tablero numero))))

aplanar :: [[Posicion]] -> [Posicion]
aplanar [] = []
aplanar t = (head t) ++ (aplanar (tail t))
 
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