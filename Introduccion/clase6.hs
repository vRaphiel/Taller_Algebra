yLogico :: Bool -> Bool -> Bool
yLogico True True = True
yLogico _ __ = False

oLogico :: Bool -> Bool -> Bool
oLogico _ __ = True
oLogico False False = False

implica :: Bool -> Bool -> Bool
implica True False = False
implica _ __ = True

sumaGaussiana :: Integer -> Integer
sumaGaussiana 0 = 0
sumaGaussiana n = n + sumaGaussiana ( n - 1 )

algunoEsCero :: (Integer, Integer, Integer) -> Bool
algunoEsCero (x,y,z) = False
algunoEsCero (0,y,z) = True
algunoEsCero (x,0,z) = True
algunoEsCero (x,y,0) = True

-- algunoEsCero (x, y, z) = z == 0 || x == 0 || y == 0

productoInterno :: (Float, Float) -> (Float,Float) -> Float
productoInterno (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

sumaDigitos :: Integer -> Integer
sumaDigitos n 
 | calcMod n == n = n
 | calcMod n /= n = calcMod n + sumaDigitos(calcDiv n)

calcMod :: Integer -> Integer
calcMod n = mod n 10

calcDiv :: Integer -> Integer
calcDiv n = n `div` 10

digitosIguales :: Integer -> Bool
digitosIguales n
 | n < 10 = True
 | otherwise = primerDigito n == segundoDigito n && digitosIguales (div n 10)
 where primerDigito n = mod n 10
       segundoDigito n = mod (div n 10) 10

--I Will Die
esSumaDeDosPrimosHasta :: Integer -> Integer -> Bool
esSumaDeDosPrimosHasta n 1 = False
esSumaDeDosPrimosHasta n k = (esPrimo k) && esPrimo (n-k) || esSumaDeDosPrimosHasta n (k-1)
---- Test
esSumaDeDosPrimos n | n <= 3 == False
esSumaDeDosPrimos otherwise = esSumaDeDosPrimosHasta n (n-1)