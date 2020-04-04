module FuncionesSimples
where

esPar :: Integer -> Bool
esPar x = (mod x 2 == 0)

esImpar :: Integer -> Bool
esImpar x = (mod x 2 /= 0)

doble :: Float -> Float
doble x = x + x

cuadruple :: Float -> Float
cuadruple x = doble (doble x)

unidades :: Integer -> Integer
unidades x = (mod (abs x) 10)

sumaUnidades :: Integer -> Integer -> Integer -> Integer
sumaUnidades x y z = unidades x + unidades y + unidades z

todosImpares :: Integer -> Integer -> Integer -> Bool
todosImpares n x y  = (esImpar n) && (esImpar x) && (esImpar y)

alMenosUnImpar :: Integer -> Integer -> Integer -> Bool
alMenosUnImpar n x y = (esImpar n) || (esImpar x) || (esImpar y)

alMenosDosIMpares :: Integer -> Integer -> Integer -> Bool
alMenosDosIMpares n x y = ((esImpar n) && (esImpar x)) || (esImpar y)

alMenosDosPares :: Integer -> Integer -> Integer -> Bool
alMenosDosPares x y z = ((esPar x) && (esPar y)) || (esPar y)

esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y = (mod x y == 0)