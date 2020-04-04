-- Funcion que calcula suma de cuadrados
-- f x y = x * x + y * y

-- Soy un comentario adorable
-- g x y z = x + y + z * z

doble x = 2 * x

triple x = 3 * x

suma x y = x + y

--Function Norma Vectorial
normaVectorial x1 x2 = sqrt ((x1 * x1) + (x2 * x2))
-- normaVectorial x1 x2 = sqrt ((x1^2) + (x2^2))
-- normaVectorial x1 x2 = sqrt ((x1**) + (x2**))
-- Cureosear ** o ^
-- normaVectorial x1 x2 = ((x1 * x1) + (x2 * x2))^(1/2) -- Tira error
-- normaVectorial x1 x2 = ((x1 * x1) + (x2 * x2))**(1/2)


moduloAux x1 x2 = mod x1 x2

-- Devuelve el cociente entre x1 y x2 (x1/x2)
cocienteDeDivision x1 x2 = div x1 x2

funcionConstante8 x = 8

respuestaATodo = (funcionConstante8 2) + 34

-- Empezando con guardas

-- IF loco
-- f n | n == 0 = 1
--	   | n /= 0 = 0

--Otherwise = si / no
-- f n | n == 0 = 1
--     | otherwise = 0

-- Error else
--	| otherwise = 0
--f n | n == 0 = 1

-- Funcion Signo
signo n | n > 0 = 1
        | n < 0 = -1
        | n == 0 = 0

--absoluto n | n == 0 = 0
--			| n < 0 = (-1 * n)
--			| n > 0 = n

absoluto n | n < 0 = 0
		   | otherwise = n

maximo n x | n > x = n
           | n < x = x
           | otherwise = n

maximoAux n x | n > x = n
            | n < x = x
            | n == x = n
            
maximo3 n x y | (maximo n x) < y = y
 			  | (maximo n x) > y = (maximo n x)
 			  | (maximo n x) == y = y
 			  | otherwise = 0
{-
maximo4 n x y | (maximo n x) < y = y || (maximo n x)
 			  | otherwise = 0
-}