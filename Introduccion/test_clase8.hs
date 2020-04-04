type Conjunto a = [a]

vacio :: Conjunto a
vacio = []

pertenece :: Eq a => a -> Conjunto a -> Bool
pertenece _ [] = False
pertenece x (y:cy) = x == y || (pertenece x cy)

agregar :: Eq a => a -> Conjunto a -> Conjunto a
agregar x cy
 | pertenece x cy = cy
 | otherwise = x:cy

{-- Ejercicios 2 --}
union :: Eq a => Conjunto a -> Conjunto a -> Conjunto a
union [] cy = cy
union (x:cx) cy = agregar x (union cx cy)

interseccion :: Eq a => Conjunto a -> Conjunto a -> Conjunto a
interseccion [] _ = []
interseccion (x:cx) cy
 | pertenece x cy = agregar x (interseccion cx cy)
 | otherwise = interseccion cx cy

incluido :: Eq a => Conjunto a -> Conjunto a -> Bool
incluido [] _ = True
incluido (x:cx) cy
 | pertenece x cy = incluido cx cy
 | otherwise = False

 -- (pertenece x cy) &&  (incluido cx cy)

iguales :: Eq a => Conjunto a -> Conjunto a -> Bool
iguales [] _ = False
iguales cx cy = (incluido cx cy) && (incluido cy cx)


{--
permutaciones :: Eq a => Conjunto a -> Conjunto [a]
permutaciones 
--}

long :: [a] -> Integer
long [] = 0
long (_:xs) = 1 + long xs

permutaciones :: Integer -> Conjunto [Integer]
permutaciones 1 = agregar [1] vacio
permutaciones n = insertarEnTodaListaEnTodaPos n (permutaciones (n-1))

-- insertarEnTodaListaEnTodaPos :: Fila -> Conjunto [Fila] -> Conjunto [Fila]
insertarEnTodaListaEnTodaPos :: Integer -> Conjunto [Integer] -> Conjunto [Integer]
insertarEnTodaListaEnTodaPos n (xs:[]) = insertarEnTodaPos n xs
insertarEnTodaListaEnTodaPos n (xs:cxs) = union (insertarEnTodaPos n xs) (insertarEnTodaListaEnTodaPos n cxs)

insertarEnTodaPos :: Integer -> [Integer] -> Conjunto [Integer]
-- hacer long
insertarEnTodaPos n xs = insertarEnTodaPosHasta n xs (long xs + 1)

insertarEnTodaPosHasta :: Integer -> [Integer] -> Integer -> Conjunto [Integer]
insertarEnTodaPosHasta n xs 1 = agregar (n:xs) vacio
insertarEnTodaPosHasta n xs k =  agregar (insertarEnPos n xs k) (insertarEnTodaPosHasta n xs (k-1))

insertarEnPos :: Integer -> [Integer] -> Integer -> [Integer]
insertarEnPos n xs 1 = n:xs -- Ya son listas
insertarEnPos n (x:xs) k = x:(insertarEnPos n xs (k-1))

-- permutaciones :: Conjunto Filas -> Conjunto [Filas]

-- permutarTablero :: Tablero -> Conjunto [Tablero]
-- cada tablero es el mismo tablero con las filas permutadas
