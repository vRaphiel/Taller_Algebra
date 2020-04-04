{--Ejercicio 2--}
sucesion :: Integer -> Integer
sucesion 1 = 1
sucesion n = terminoenesimo (n-1)

terminoenesimo :: Integer -> Integer
terminoenesimo 1 = 1
terminoenesimo 2 = 2
terminoenesimo n = 1 + sumatorian n

sumatorian :: Integer -> Integer
sumatorian 1 = 1
sumatorian n = ((n-1) * (terminoenesimo (n-1))) + sumatorian (n-1)

{--Ejercicio 5--}
{- esSemiPerfecto :: Integer -> Bool
esSemiPerfecto 1 = True
esSemiPerfecto 2 = False
esSemiPerfecto n = sumaAux n (n-1) 0 -}

{- sumaAux :: Integer -> Integer -> Integer -> Bool
sumaAux numero divisor suma
 | suma == numero = True
 | divisor == 0 = False
 | mod numero divisor == 0 = sumaAux numero (divisor-1) (suma + divisor)
 | otherwise = sumaAux numero (divisor-1) suma -}

castToList :: Integer -> Integer -> [Integer] -> Bool
castToList numero divisor listitaAuxiliar
 | divisor == 0 = tda listitaAuxiliar numero
 | mod numero divisor == 0 = castToList numero (divisor-1) (divisor:listitaAuxiliar)
 | otherwise = castToList numero (divisor-1) listitaAuxiliar

tda :: [Integer] -> Integer -> Bool
tda lista numero
 | length lista == 0 = False
 | (sumaTda (tail lista) (head lista) numero) = True
 | otherwise = tda (tail lista) numero

sumaTda :: [Integer] -> Integer -> Integer -> Bool
sumaTda lista nSum numero
 | length lista == 0 = False
 | ((head lista + nSum) == numero) = True
 | otherwise = sumaTda (tail lista) (nSum+(head lista)) numero
