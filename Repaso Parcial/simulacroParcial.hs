-- Simulacro de Parcial Taller de Algebra
{--
Ejercicio 1
Implementar la funcion 
menorLex :: (Float, Float, Float) -> (Float, Float, Float) -> Bool
que datos dos vectores x,y e R3 decida si es x menor a y en el sentido lexografico 
--}

menorLex :: (Float, Float, Float) -> (Float, Float, Float) -> Bool
menorLex (x1,y1,z1) (x2,y2,z2) 
 | (x1 <= x2) = True
 | (x1 <= x2) && (y1 <= y2) = True
 | (x1 <= x2) && (y1 <= y2) && (z1 <= z2) = True
 | otherwise = False

{--
Ejercicio 2
Implementar una funcion
sumaFibonacci :: Integer -> Integer
que para cana n >= 1 calcule (sumatoria hasta n de j = 0 de f(n) donde
f(n) es el n-ésimo termino de la sucesion Fibonacci
--}
--sumaFibonacci :: Integer -> Integer


{--
Ejercicio 3
Implementar la funcion esDefectivo :: Integer -> Bool que dado un n e N>0
determine si es defectivo, lo cual <=> la suma de los divisores positivos de n es menor que n
--}

esDefectivo :: Integer -> Bool
esDefectivo n 
 | n > (sumaDeDivisores n (n-1)) = True
 | otherwise = False

sumaDeDivisores :: Integer -> Integer -> Integer
sumaDeDivisores n m
 | m == 0 = 1
 | m <= 1 = 1
 | (mod n m) == 0 = m + (sumaDeDivisores n (m-1))
 | otherwise = (sumaDeDivisores n (m-1))

{--
Ejercicio 4
Programar la funcion 
maximaDistancia :: [Integer] -> Integer
que determina cual es la maxima distancia entre dos 
elementos consecutivos en una lista de numeros enteros 
--}
maximaDistancia :: [Integer] -> Integer
maximaDistancia [x1,x2] = absoluto (x1-x2)
maximaDistancia (x1:x2:xs) = maximaDistanciaAuxiliar a 0

maximaDistanciaAuxiliar :: [Integer] -> Integer -> Integer
maximaDistanciaAuxiliar a b 
 | length a == 1 = b
 | b > ((head a) - (head (tail a))) = maximaDistanciaAuxiliar (tail a) absoluto(b)
 | b < ((head a) - (head (tail a))) = maximaDistanciaAuxiliar (tail a) absoluto(head a - head(tail a))

{--
Ejercicio 5
Programar la funcion 
comprimir :: [Integer] -> [(Integer),(Integer)]
que dada una lista de numeros enteros devuelva una lista que contenga 
una tupla (numero, cantidad de apariciones) por cada rafaga de numeros iguales adyacentes
Sugerencia: Empezar reemplazando cada numero n por una tupla (n,1)
--}
--Consultar
comprimir :: [Integer] -> [(Integer, Integer)]
comprimir [] = []
comprimir (x:xs) = (armarTupla (x:xs) 1):comprimir (elimiarRepetidosAlPrincipio (x:xs) x)

elimiarRepetidosAlPrincipio :: [Integer] -> Integer -> [Integer]
elimiarRepetidosAlPrincipio [] y = []
elimiarRepetidosAlPrincipio (x:xs) y | x == y = elimiarRepetidosAlPrincipio xs y
									 | otherwise = (x:xs)
--Siempre empezar por 1
armarTupla :: [Integer] -> Integer -> (Integer, Integer)
armarTupla lista cantidad
 | length lista == 1 = (head lista, cantidad)
 | length lista > 1 && sonIguales (head lista) (head (tail lista)) == True =  armarTupla (tail lista) (cantidad + 1)
 | length lista > 1 && sonIguales (head lista) (head (tail lista)) == False = ((head lista), (cantidad))

sonIguales :: Integer -> Integer -> Bool
sonIguales x y 
 | x == y = True
 | otherwise = False



sumaDeDigitos :: Integer -> Integer
sumaDeDigitos n 
 | n < 10 = n
 | (mod n 10) == 0 = (div n 10)
 | n > 10 = (mod n 10) + sumaDeDigitos(div n 10)
