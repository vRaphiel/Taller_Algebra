module FuncionesComplejas
where

import FuncionesSimples

r1 :: Integer -> Integer -> Bool
r1 x y = (esPar x == esPar y)
-- Suma x + y es par

r2 :: Integer -> Integer -> Bool
r2 x y = (esMultiploDe ((2*x) + (3*y)) 5)

r3 :: Integer -> Integer -> Bool
r3 x y = (unidades x /= unidades y) && (unidades (x * y) /= unidades x) && (unidades (x * y) /= unidades y)

r4 :: Integer -> Integer -> Bool
r4 x y = ((x < 3) && (y < 3)) || ((x>= 3 && y >= 3))

r5 :: Integer -> Integer -> Bool
r5 x y = ((x < 3) && (y < 3)) || (((x>= 3 && x <7) && (y >= 3 && y < 7))) || ((x>= 7 && y >= 7)) 

r6 :: (Integer, Integer) -> (Integer, Integer) -> Bool
r6 (a, b) (p,q) = ((a * q) == (b * p)) && ((p/=0) && (esMultiploDe a p)) || ((q/=0) && (esMultiploDe b q))

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

facto :: Integer -> Integer
facto n
 | n == 0 = 1
 | n > 0 = n * facto (n-1)
 | otherwise = (-1)

--Change model to division
r7 :: (Integer, Integer) -> (Integer, Integer) -> Bool
r7 (a, b) (p,q) = ((a * q) == (b * p))