-- Recursion
-- #DepressionIncomingNoForInHaskell

-- Fibbonacci Function
-- Fib from Z>0 -> Z
import FuncionesComplejas
import FuncionesSimples


fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)


{-
fibonacci :: Integer -> Integer
fibonacci n 
 | n == 0 = 0
 | n == 1 = 1
 | n > 1 = fibonacci(n-1) + fibonacci(n-2)
 | otherwise = (-1)
-}

-- Sucesion Random
--functiona :: Integer -> Integer
functionAnesimoA :: Integer -> Integer
functionAnesimoA 1 = 2
functionAnesimoA n = (2^n)*factorial(n);

--Sucesion 1
functionA :: Integer -> Integer
functionA 0 = 0
functionA n = (2*n*functionAnesimoA(n)) + ((2^(n+1))*factorial(n))
--functionA n = (2*(n-1)*functionAnesimoA(n-1) + (2^(n-1)*factorial(n-1))

--Sucesion 2
functionB :: Integer -> Integer
functionB 1 = 1
functionB 2 = 2
functionB n = ((n-2)*functionB(n-1)) + (2*(n-1)*functionB(n-2))

--Sucesion 3
functionC :: Integer  -> Integer
functionC 1 = (-3)
functionC 2 = 6
functionC n 
 | esImpar n = ((-1)*(functionC (n-1))) -3
 | esPar n = functionC (n-1) + 2*(functionC(n-2)) + 9 

--Sumatoria 1
functionSumA :: Integer -> Integer
functionSumA 0 = 1
functionSumA n = 2^n + functionSumA(n-1)

--Sumatoria 2
functionSumB :: (Integer, Float) -> Float
functionSumB (0, q) = 0
functionSumB (n,q) = q^n + (functionSumB(n-1, q))

--Sumatoria 3
functionSumC :: (Integer, Float) -> Float
functionSumC (n,q) = functionSumB(2*n, q)

-- sum3 (n,q) | n == 0 = 0 
-- | n==1 = q + q^2
-- | n > 1 = sum3(n-1,q) + q^(2*n-1) + 

--Sumatoria 4
functionSumD :: (Integer, Float) -> Float
functionSumD (n,q) 
 | n == 0 = 1
 | n == 1 = q + q^2
 | otherwise = functionSumD(n-1, q) + q ^ (2 * n - 1) + q ^ (2 * n) 

-- Determina si un numero es multiplo de 3
esMultiploDeTres :: Integer -> Bool
esMultiploDeTres n  
 | n == 0 = True
 | n >= 3 = esMultiploDeTres (n - 3)
 | otherwise = False


-- Rerevisar

iesimoImpar :: Integer -> Integer
iesimoImpar n = 2*n - 1

functionSumaImpares :: Integer -> Integer
functionSumaImpares n | n == 1 = 1
					  | n > 1 = functionSumaImpares (n - 1) + iesimoImpar n

-- medioFact
medioFact :: Integer -> Integer
medioFact n | n == 1 = 1
			| n == 2 = 2
			| n > 2 = n * medioFact(n - 2)

-- Recursiva
notFinishAtNegative :: Integer -> Integer
notFinishAtNegative n | n < 0 = notFinishAtNegative n
			| n >= 0 = 0