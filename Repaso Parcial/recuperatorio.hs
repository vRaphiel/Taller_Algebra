{-- Ejercicio 1
Definir una funcion
mayorDistancia :: (Integer, Integer, Integer) -> Integer que dada una tupla (a,b,c) de
numeros enteros devuelva la maxima distancia de todas las funciones que escriba
--}

mayorDistancia :: (Integer, Integer, Integer) -> Integer
mayorDistancia (0,0,0) = 0
mayorDistancia (a,b,c) 
 | (absVal (a - b) > absVal (a - c)) && (absVal (a - b) > absVal(b - c)) = absVal(a - b)
 | (absVal (a - b) > absVal (b - c)) && (absVal (a - b) > absVal(b - c)) = absVal(a - b)
 | (absVal (b - c) > absVal (a - c)) && (absVal (b - c) > absVal(a - b)) = absVal(b - c)  
 | otherwise = 0

absVal :: Integer -> Integer
absVal a 
 | a == 0 = 0
 | a < 0 = ((-1) * a)
 | otherwise = a

{--Ejercicio 2
Implementar una funcion
suma :: Integer -> Integer
que para neN>0 calcule la sumatoria (Sum desde i = 1 hasta 2n de = (i**2 + 2i)--}
suma :: Integer -> Integer
suma 0 = 0
suma a 
 | a < 0 = (-1)
 | a > 0 = sumaRandom (2*a)

sumaRandom :: Integer -> Integer
sumaRandom a
 | a > 0 = (a^2 + 2*a) + (sumaRandom (a-1))
 | a == 0 = a

 {-- Ejercicio 3
 Implementar una funcion
 cantCambiosParidad :: [Integer] -> Integer
 que dada una lista de enteros retorne cuantas veces cambia de par a impar
 --}
cantCambiosParidad :: [Integer] -> Integer
cantCambiosParidad [] = 0
cantCambiosParidad a = sumaParidad a 0

sumaParidad :: [Integer] -> Integer -> Integer
sumaParidad lista numero
 | (length lista) <= 1 = numero 
 | (length lista) > 1 && (cambioParidad (head lista) (head (tail lista))) = sumaParidad (tail lista) (numero + 1)
 | (length lista) > 1 && not (cambioParidad (head lista) (head (tail lista))) = sumaParidad (tail lista) (numero)

cambioParidad :: Integer -> Integer -> Bool
cambioParidad a b 
 | (mod a 2) == (mod b 2) = False
 | (mod a 2) /= (mod b 2) = True
 | otherwise = False

 longitud :: [Integer] -> Integer
 longitud [] = 0
 longitud (_:xs) = 1 + longitud xs

 imparesHasta :: Integer -> [Integer]
 imparesHasta 0 = []
 imparesHasta n | n `mod` 2 == 1 = n : imparesHasta (n-1)
  | otherwise = imparesHasta (n-1)

 multiplosDe7 :: [Integer] -> [Integer]
 multiplosDe7 [] = []
 multiplosDe7 (x:xs) | x `mod` 7 == 0 = x : (multiplosDe7 xs)
  | otherwise = multiplosDe7 xs