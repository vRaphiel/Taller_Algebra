import FuncionesComplejas
import FuncionesSimples

eAprox :: Integer -> Float
eAprox 0 = 1
eAprox n = (1 / (fromInteger( factorial n ))) + eAprox (n-1)

e :: Float
e = eAprox 100

parteEntera :: Float -> Integer
parteEntera n
	| n < 1 && n > 0 = 0
	| n > 1 = 1 + parteEntera ( n - 1 )
	| n < 1 = parteEntera ( n + 1 ) -1

parteDecimal :: Float -> Float
parteDecimal n 
	 | (n < 1 && n > 0) = n
	 | n > 1 = parteDecimal ( n - 1 )

division :: Integer  -> Integer  -> (Integer , Integer)
division a d 
    | (a < d && a >= 0) = (0, a)
    | (d > 0 && a > 0) = (fst qr'+ 1, snd qr')
    | (d > 0 && a < 0) = (fst (division (a+d) d) - 1, snd (division (a+d) d))
 	where qr'= division (a-d) d


-- ¿Que Hice?
-- Revisar
{-
sumaDeDivisoresHasta :: Integer -> Integer
sumaDeDivisoresHasta 0 = 0
sumaDeDivisoresHasta n = sumaDivisores n (div n 2)

sumaDivisores :: Integer -> Integer -> Integer
sumaDivisores a b
 | (a > 0 && b > 0) && (mod a b == 0) = a + (sumaDivisores a (b-1))
 | otherwise = (sumaDivisores a (b-1))
-}

sumaDivisoresHasta :: Integer -> Integer -> Integer
sumaDivisoresHasta n k | k == 0 = 0
					   | mod n k == 0 = k + sumaDivisoresHasta n (k -1)
					   | otherwise = sumaDivisoresHasta n (k - 1)

sumaDivisores :: Integer -> Integer
sumaDivisores n = sumaDivisoresHasta n n

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | n == k = n
					  | mod n k == 0 = k
					  | otherwise = menorDivisorDesde n (k + 1)

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

esPrimo :: Integer -> Bool
esPrimo n = menorDivisor n == n

sumaExponentes :: Integer -> Integer -> Integer
sumaExponentes m k | m == 1 = k
				   | m > 1 = k^m + sumaExponentes (m-1) k

f :: Integer -> Integer -> Integer
f n m | n == 1 = sumaExponentes m 1
	  | n > 1 = sumaExponentes m n + f (n-1) m


sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q n m 

{-
-- 1 <= a <= n
-- 1 <= b <= m
-}