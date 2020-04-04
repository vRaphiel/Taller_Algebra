{--Ejercicio 1--}
esFibonacci :: Integer -> Bool
esFibonacci 0 = True
esFibonacci 1 = True
esFibonacci n = auxFibonacci n 1

fib :: Integer -> Integer
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib(n-2)

auxFibonacci :: Integer -> Integer -> Bool
auxFibonacci numero contador
 | contador > numero = False
 | (fib contador) == numero = True
 | otherwise = auxFibonacci numero (contador + 1)


{--
Ejercicio 3
--}
{- longitudCamino :: [(Integer, Integer)] -> Float
longitudCamino [] = 0
longitudCamino (x:[]) = 0
longitudCamino n = distancia (head n) (head (tail n)) + longitudCamino (tail n)

distancia :: (Integer, Integer) -> (Integer, Integer) -> Float
distancia a b = sqrt ( ((fst b) - (fst a))^2 + ((snd b) - (snd a))^2 ) 
--}
{-- longitudCamino :: [(Integer, Integer)] -> Float
longitudCamino [] = 0
longitudCamino (x:[]) = 0
longitudCamino (x:xs) = longitudAux x xs

longitudAux :: (Integer, Integer) -> [(Integer, Integer)] -> Float
longitudAux a (x:[]) = sqrt ( ((fst x) - (fst a))^2 + ((snd x) - (snd a))^2 ) 
 --}

{-- Funcion Reversa --}
rev1 :: [Integer] -> [Integer] 
rev1 []  = [] 
rev1 (x:xs) = rev1 xs ++ [x]

{-- Ejercicio 5--}
incmin :: [Integer] -> [Integer]
incmin [] = []
incmin (x:[]) = [x+x]
incmin lista = sumarN lista (encontrarMenor (tail lista) (head lista))

encontrarMenor :: [Integer] -> Integer -> Integer
encontrarMenor lista numero
 | length lista == 0 = numero
 | head lista < numero = encontrarMenor (tail lista) (head lista)
 | otherwise = encontrarMenor (tail lista) numero

sumarN :: [Integer] -> Integer -> [Integer] 
sumarN [] a = []
sumarN (x:xs) a = (x+a):(sumarN xs a)