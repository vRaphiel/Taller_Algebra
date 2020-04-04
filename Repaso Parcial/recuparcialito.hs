{-- Ejercicio 1--}
f :: Integer -> Integer
f n
 | mod n 5 == 0 = n^2
 | otherwise = n - 1

g :: Integer -> (Integer, Integer)
g n = (3*(f n), (f n)^3)

h :: Integer -> (Integer, Integer)
h n = g n

{-- Ejercicio 2 --}
esImpar :: Integer -> Bool
esImpar 0 = False
esImpar 1 = True
esImpar n 
 | mod n 2 == 1 = True
 | otherwise = False

soloImpares :: Integer -> Bool
soloImpares 0 = False
soloImpares 1 = True
soloImpares n
 | n < 10 && esImpar n = True
 | n >= 10 && esImpar (mod n 10) = soloImpares (div n 10)
 | otherwise = False

{-- Ejercicio 3 --}
s :: Integer -> Integer -> Integer
s n 0 = sumatoria n
s 0 m = productoria m
s n m = sumatoria n + productoria m

sumatoria :: Integer -> Integer
sumatoria 1 = 1
sumatoria n = (n^3) + sumatoria (n-1)

productoria :: Integer -> Integer
productoria 1 = 3
productoria m = ((m^2) + (2*m)) + productoria (m-1)

{--Ejercicio 4--}
piAprox :: Integer -> Float
piAprox 0 = 0
piAprox n = serieGeorgeLeibniz n 0
serieGeorgeLeibniz :: Integer -> Float -> Float
serieGeorgeLeibniz n cuenta
 | n == 1 = (4 + cuenta)
 | mod n 2 == 0 = serieGeorgeLeibniz (n-1) (cuenta + (-1)*(4/fromInteger((2*n)-1)))
 | otherwise = serieGeorgeLeibniz (n-1) (cuenta + (4/fromInteger((2*n)-1)))

{-- Ejercicio 5 --}
sumaRacionales :: Integer -> Integer -> Float
sumaRacionales a b = sumaAux (fromInteger(a)) (fromInteger(b)) (fromInteger(a))

sumaAux :: Float -> Float -> Float -> Float
sumaAux a b referencia
 | b == 0 = 0
 | a == 0 =  0 + (sumaAux referencia (b-1) referencia)
 | otherwise = (a / b) + (sumaAux (a-1) b referencia)