--f x = pi
--f y = not y

--doble :: Integer -> Integer
doble :: Float -> Float
doble x = x + x

--cuadruple :: Integer -> Integer
cuadruple :: Float -> Float
cuadruple x = doble (doble x)

dist :: Float -> Float -> Float -> Float -> Float
dist w x y z = w + x + y + z
--Type undefined because it cannot recieve other value than Float (If i insert Int/Integer for example)

esPar :: Integer -> Bool
esPar x = (mod x 2 == 0)

esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y = (mod x y == 0)

identidad :: a -> a
identidad x = x

{-
triple :: Integer -> Integer
triple x = x * 3
-}

--triple :: Num a => a -> a
triple x = x * 3


--(Notacion Infija - Solo para funciones de 2 parametros)
--(^) 2.5 2 

--5 `mod` 2

--mindBound (Smaller number that i can print)
--maxBound (Bigger number that i can print)
--float cannot divide with `div`, i may use /

crearPar :: Integer -> Integer -> (Integer, Integer) 
crearPar x y = (x,y)

invertir :: Integer -> Integer -> (Integer, Integer)
invertir x y = (y,x)

--test normaVectorial in distanciaPuntos
distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x, y) (w, z) = sqrt( (w-x)^2 + (z-y)^2 )

normaVectorial :: (Float, Float) -> Float
normaVectorial p = sqrt ((fst p) ^ 2 + (snd p) ^ 2)
-- Quedan los ejercicios de la guia para hacer