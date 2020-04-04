-- Listas
{-
a : [a]
5 : [1,2]
3.4 : []
-}


--listar a b c = a:b:c[]
listar :: a -> a -> a -> [a]
listar a b c = [a,b,c]
{-
listaResta 
-- [1,0..-100]
-}

sumatoria :: [Integer] -> Integer
sumatoria a 
 | length a > 0 = (head a) + (sumatoria (tail a))
 | length a == 0 = 0
 | otherwise = 0

sumatoria2 :: [Integer] -> Integer
sumatoria2  [] = 0
sumatoria2 (x:xs) = sumatoria  xs + x

pertenece :: Eq a => a -> [a] -> Bool
pertenece a b
 | length b > 0 && a == head b = True
 | length b == 0 = False
 | otherwise = pertenece a (tail b)

longitud :: [a] -> Integer
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

pertenece2 :: Eq a => a -> [a] -> Bool
pertenece2 a [] = False
pertenece2 a b
 | length b == 0 = False
 | a == head b = True
 | otherwise = pertenece2 a (tail b)

productoria :: [Integer] -> Integer
productoria a 
 | length a > 0 = (head a) * (productoria (tail a))
 | length a == 0 = 1
 | otherwise = 1

 --Maximo
calculaMaximoPorLista :: Integer -> [Integer] -> Integer
calculaMaximoPorLista a b 
 | length b > 0 && a > (head b) = calculaMaximoPorLista a (tail b)
 | length b > 0 && a < (head b) = calculaMaximoPorLista (head b) (tail b)
 | length b == 0 = a

maximo :: [Integer] -> Integer
maximo a = calculaMaximoPorLista (head a) (tail a)

rara :: [Integer] -> Integer
rara [] = 0
rara (x:[]) = 1
rara (x:y:[]) = x + y
rara (x:y:xs) = x * y + rara xs
-- rara [4,3,5]  => x -> 4, y-> 3, xs ->[5]

sumarN :: Integer -> [Integer] -> [Integer]
sumarN a [] = []
sumarN a (x:xs) = (a+x):(sumarN a xs)

sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero (x:xs) = (x+x):(sumarN x xs)

retornarUltimoElemento :: [Integer] -> Integer
retornarUltimoElemento a 
	| (length a > 1) = retornarUltimoElemento (tail a)
	| (length a == 1) = head a
	| otherwise = 1

sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo (x:xs) = (retornarUltimoElemento xs + x):(sumarN (retornarUltimoElemento xs) xs)

pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs) 
 | mod x 2 == 0 = x:(pares xs)
 | otherwise = pares xs

multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN n [] = []
multiplosDeN n (x:xs)
 | mod x n == 0 = x:(multiplosDeN n xs)
 | otherwise = multiplosDeN n xs

quitar :: Integer -> [Integer] -> [Integer]
quitar n [] = []
quitar n (x:xs)
 | n == x = xs
 | otherwise = x:(quitar n xs)

quitarTodos :: Integer -> [Integer] -> [Integer]
quitarTodos n [] = []
quitarTodos n (x:xs)
 | n == x = quitarTodos n xs
 | otherwise = x:(quitarTodos n xs)

-- Falta hayRepetidos, eliminarRepetidos, ordenar y maximo
-- Ordenar, con maximo o, como recomendacion, minimo
-- Reverso

hayRepetidos [] = False
hayRepetidos (x:xs)
 | pertenece x xs = True
 | otherwise = hayRepetidos xs

--reverso :: [Integer] -> [Integer]
--reverso [] = []
--reverso (x:xs) = (reverso xs)++[x]

agregarAlFInal x [] = [x]
agregarAlFinal x (y:ys) = y:(agregarAlFInal x ys)

reverso [] = []
reverso (x:xs) = agregarAlFInal x (reverso xs)

-- Ordenar Aplicar metodo burbuja


ordenar [] = []
ordenar xs = ordenar (quitar maxxs xs) ++ [maxxs]
where maxxs = maximo xs

maximo [x] = x
maximo (x:xs) = 

max::Integer -> Integer -> Integer
max x y 
 | x >= y = x
 | otherwise = y