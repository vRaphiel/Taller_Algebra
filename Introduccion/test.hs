type Conjunto a = [a]
type Fila = [Integer]
type Tablero = [Fila]
type Posicion = (Integer,Integer)
type Camino = [Posicion]

sopa1 :: Tablero
sopa1 = [[13,12,6,4],[1,1,32,25],[9,2,16,8],[7,3,5,4],[1,2,8,2]]

aplanar :: Tablero -> [Integer]
aplanar [] = []
aplanar t = (head t) ++ (aplanar (tail t))

tuplar :: [Integer] -> [(Integer, Integer)]
tuplar [] = []
tuplar (x:xs) = (x, 1):(tuplar xs)

comprimirPrimero :: [(Integer, Integer)] -> [(Integer, Integer)]
comprimirPrimero [x] = [x]
comprimirPrimero ((x1, y1):(x2,y2):xs) | x1 == x2 = comprimirPrimero ((x1, y1 + y2):xs)
                                       | otherwise = comprimirPrimero ((x1, y1):xs)
                                       
deslistar :: [a] -> a
deslistar [a] = a

quitarTupla :: (Integer, Integer) -> [(Integer, Integer)] -> [(Integer, Integer)]
quitarTupla _ [] = []
quitarTupla n (x:xs)
 | n == x = quitarTupla n xs
 | otherwise = x:(quitarTupla n xs)

comprimir :: [(Integer, Integer)] -> [(Integer, Integer)]
comprimir [] = []
comprimir (x:xs) = (deslistar (comprimirPrimero (x:xs))):(comprimir (quitarTupla x (x:xs)))

masApariciones :: [(Integer, Integer)] -> Integer
masApariciones [(x1, x2)] = x1
masApariciones ((x1, x2):(y1, y2):xs)
 | x2 >= y2 = masApariciones ((x1, x2):xs)
 | otherwise = masApariciones ((y1, y2):xs)
 
masRepetidoLista :: [Integer] -> Integer
masRepetidoLista xs = masApariciones (comprimir (tuplar xs))

masRepetido :: Tablero -> Integer
masRepetido t = masRepetidoLista (aplanar t)
                                       