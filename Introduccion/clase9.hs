{--Clase 9--}
{-- Algoritmo de Euclides --}

{--Algoritmo de division => a = qb + r--}
mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (mod a b)

mcd2 :: Integer -> Integer -> Integer
mcd2 a b = maxDivMenorQue a b (min a b)

maxDivMenorQue :: Integer -> Integer -> Integer -> Integer
maxDivMenorQue a b i
 | ((mod a i) == 0 ) && ((mod b i) == 0) = i
 | otherwise = maxDivMenorQue a b (i-1)

emcd :: Integer -> Integer -> (Integer, Integer, Integer)
emcd a 0 = (a, 1, 0)
emcd a b = (g, s, t)
 where  (g, s', t') = emcd b (mod a b)
        s = t' 
        t = s' - t'*q
        q = div a b

tieneSolucion :: Integer -> Integer -> Integer -> Bool
tieneSolucion a b m = mod b (mcd a m) == 0

solucionParticular :: Integer -> Integer -> Integer -> Integer
solucionParticular a b m
 | tieneSolucion a b m = s * (div d (mcd a m))
 where s = snd3 (emcd a m)

{- solucionGeneral :: Integer -> Integer -> Integer -> (Integer, Integer)
solucionGeneral a b m
 | 
 -}