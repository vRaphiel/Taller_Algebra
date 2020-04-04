{--
Ejercicio 3
--}

esBSuave :: Integer -> Integer -> Bool
esBSuave n r = aux (calcularDivPrimos n (n-1)) r

calcularDivPrimos :: Integer -> Integer -> [Integer]
calcularDivPrimos n 1 = []
calcularDivPrimos n divisor
 | (mod n divisor == 0) && (esPrimo divisor) = divisor:(calcularDivPrimos n (divisor -1))
 | otherwise = calcularDivPrimos n (divisor -1)

aux :: [Integer] -> Integer -> Bool
aux lista numero
 | length lista == 0 = True
 | (head lista) < numero = aux (tail lista) numero
 | otherwise = False

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | n == k = n
 | mod n k == 0 = k
 | otherwise = menorDivisorDesde n (k + 1)

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

esPrimo :: Integer -> Bool
esPrimo 1 = True
esPrimo n = menorDivisor n == n

{--Ejercicio 4--}
congruenciaMod3 :: [Integer] -> (Integer, Integer, Integer)
congruenciaMod3 [] = (0,0,0)
congruenciaMod3 lista = congruencia lista (0,0,0)

congruencia :: [Integer] -> (Integer, Integer, Integer) -> (Integer, Integer, Integer)
congruencia lista (a,b,c) 
 | length lista == 0 = (a,b,c)
 | mod (head lista) 3 == 0 = congruencia (tail lista) (a+1,b,c)
 | mod (head lista) 3 == 1 = congruencia (tail lista) (a,b+1,c)
 | mod (head lista) 3 == 2 = congruencia (tail lista) (a,b,c+1)
 | otherwise = congruencia (tail lista) (a,b,c)

{--
Ejercicio 5
--}
esSumaMod7DeDos :: [Integer] -> Integer -> Bool
esSumaMod7DeDos [] _ = False
esSumaMod7DeDos lista numero = tda lista numero

tda :: [Integer] -> Integer -> Bool
tda lista numero
 | length lista == 0 = False
 | tda2 (quitarDeLista lista (head lista)) (head lista) numero = True
 | otherwise = tda (tail lista) numero

tda2 :: [Integer] -> Integer -> Integer -> Bool
tda2 lista sumN result
 | length lista == 0 = False
 | (mod ((head lista) + sumN ) 7) == result = True
 | otherwise = tda2 (tail lista) sumN result

quitarDeLista :: [Integer] -> Integer -> [Integer]
quitarDeLista [] _ = []
quitarDeLista (x:xs) n
 | x == n = xs
 | otherwise = x:(quitarDeLista xs n)