

fibo :: Integer -> Integer
fibo n | n == 0 = 0
       | n == 1 = 1
       | otherwise = fibo(n-1) + fibo(n-2)


longitud :: (Eq t) => [t] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

ultimo :: (Eq t) => [t] -> t
ultimo (x:xs) | longitud (x:xs) == 1 = x 
              | otherwise = ultimo xs

principio :: (Eq t) => [t] -> [t]
principio (x:xs) | longitud (x:xs) == 1 = [] 
                 | otherwise = x:principio xs


reverso :: (Eq t) => [t] -> [t]
reverso (x:xs)| longitud (x:xs) == 1 = [x]
              | otherwise = reverso xs ++ [x]        

primero :: (Eq t) => [t] -> t
primero (x:xs) = x

primero2 :: [Char] -> Char
primero2 [] = ' '
primero2 (x:xs) | x == ' ' = primero2 xs
               | otherwise = x
--2.1-------------------------------------------------------------------------------------------------------------------------------------------

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _[] = False 
pertenece x (y:ys) | x == y = True
                   | otherwise = pertenece x ys   

todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = False
todosIguales (x:y:xs) | x /= y = False
                      | longitud xs == 0 = True
                      | otherwise = todosIguales xs

todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [] = True
todosDistintos [x] = True
todosDistintos (x:xs) | pertenece x xs = False
                      | otherwise = todosDistintos xs

--2.4--
repetidos :: (Eq t) => [t] -> Bool
repetidos [] = False
repetidos [x] = False
repetidos (y:ys) | pertenece y ys = True
                 | otherwise = repetidos ys --aplico recursion para eliminar el primer elemento de la lista y probar con el siguiente

--2.5--
quitar :: (Eq t) => t -> [t] -> [t]
quitar x [] = []
quitar x (y:ys) | x == y = ys
                | otherwise = y:quitar x ys

quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos x [] = []
quitarTodos x (y:ys) | x == y = quitarTodos x ys
                     | otherwise = y:quitarTodos x ys

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | pertenece x xs = x:quitarTodos x (eliminarRepetidos xs)
                         | otherwise = x:eliminarRepetidos xs

-- otra forma (mejor) --

eliminarRepetidos2 :: (Eq t) => [t] -> [t]
eliminarRepetidos2 [] = []
eliminarRepetidos2 (x:xs) = x:eliminarRepetidos(quitarTodos x xs)


--funciona si son del mismo tamaño--
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [] [] = True
mismosElementos (x:xs) (y:ys) | pertenece x (y:ys) && pertenece y (x:xs) = mismosElementos xs ys
                              | otherwise = False

--funciona para toodos los casos--
mismosElementos2 :: Eq t => [t] -> [t] -> Bool
mismosElementos2 [] [] = True
mismosElementos2 (x:[]) [] = False
mismosElementos2 [] (y:[]) = False
mismosElementos2 (x:xs) [] = False
mismosElementos2 [] (y:ys) = False
mismosElementos2 (x:[]) (y:[]) 
 | x == y = True
 | otherwise = False
mismosElementos2 (x:xs) (y:[]) = False
mismosElementos2 (x:[]) (y:ys) = False
mismosElementos2 (x:xs) (y:ys)
 | x == y = mismosElementos2 xs ys
 | otherwise = False


capicua :: (Eq t) => [t] -> Bool
capicua [] = True
capicua (x:xs) | (x:xs) == reverso (x:xs) = True
               | otherwise = False

sas :: (Eq t) => [t] -> t
sas (x:xs) = head(reverso(x:xs))

--3------------------------------------------------------------------------------------------------------------------------------------------

sumatoria :: [Integer] -> Integer
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

productoria :: [Integer] -> Integer
productoria [] = 1
productoria (x:xs) = x * productoria xs


maxi :: [Int] -> Int
maxi [] = 0
maxi [y] = y
maxi (y:x:ys) | y > x = maxi (y:ys)
              | otherwise =  maxi (x:ys) --uso (y:x:ys) para poder chequear los primeros 2 numeros de la lista

sumarN :: Integer -> [Integer] -> [Integer]
sumarN _ [] = []
sumarN x (y:ys) = (x+y):sumarN x ys

sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero [] = []
sumarElPrimero (x:xs) = sumarN x (x:xs)

sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo [] = []
sumarElUltimo (x:xs) = sumarN (ultimo(x:xs)) (x:xs)

pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs) | mod x 2 == 0 = x:pares xs 
             | otherwise = pares xs

multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN _ [] = []
multiplosDeN n (x:xs) | mod n x == 0 = x:multiplosDeN n xs 
                      | otherwise = multiplosDeN n xs

--3.9--
ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar ys = ordenar(quitar (maxi ys) ys) ++[maxi ys]


--4-----------------------------------------------------------------------------------------------------------------------------

sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos (x:[]) = [x]
sacarBlancosRepetidos (x:y:xs) | x == y && x == ' ' = sacarBlancosRepetidos (y:xs)
                               | otherwise = x:sacarBlancosRepetidos (y:xs)



blancoFinal :: [Char] -> [Char]
blancoFinal [] = []
blancoFinal (x:[]) | x == ' ' = []
                   | otherwise = [x]
blancoFinal (x:xs) = x:blancoFinal xs

blancoInicio :: [Char] -> [Char]
blancoInicio [] = []
blancoInicio (x:xs) | x == ' ' =   blancoInicio xs
                    | otherwise = (x:xs)

contarEspacios :: [Char] -> Integer
contarEspacios [] = 0
contarEspacios [x] = 1
contarEspacios (x:xs) | x == ' ' = 1 + contarEspacios xs
                      | otherwise = contarEspacios xs

contarPalabras :: [Char] -> Integer
contarPalabras x = contarEspacios(blancoInicio(blancoFinal(sacarBlancosRepetidos x)))
---------------------
limpiarPalabras :: [Char] -> [Char]
limpiarPalabras x = blancoInicio(blancoFinal(sacarBlancosRepetidos x))


primeraPalabra :: [Char] -> [Char]
primeraPalabra [] = []
primeraPalabra (x:xs) | x ==' ' =[]
                      | otherwise = x:primeraPalabra xs

sacarPrimeraPalabra :: [Char] -> [Char]
sacarPrimeraPalabra [] =[]
sacarPrimeraPalabra (x:xs) | x==' ' = xs
                           | otherwise = sacarPrimeraPalabra xs



palabra :: [Char] -> [[Char]]
palabra [] = []
palabra x = primeraPalabra (x): palabra(sacarPrimeraPalabra (x))


--------5---------------------------------------------------------------------------------------------------------------------------------

sacarUltimo :: (Num t) => [t] -> [t]
sacarUltimo [] = []
sacarUltimo [x] = []
sacarUltimo (x:xs) = x:sacarUltimo xs


sumarAnteriores :: (Num t) => [t] -> t
sumarAnteriores [] = 0
sumarAnteriores (x:xs) = x + sumarAnteriores xs

sumaAcumulada :: (Num t) => [t] -> [t]
sumaAcumulada [] = []
sumaAcumulada x = sumaAcumulada (sacarUltimo x) ++ [sumarAnteriores x]


