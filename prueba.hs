import Data.Char

esMinuscula :: Char -> Bool
esMinuscula c = (ord c >= 97) && (ord c <= 122) --si c pertenece al abecedario ingles minuscula

pertenece :: Eq a => a -> [a] -> Bool
pertenece x [] = False
pertenece x xs
    | x == head xs = True
    | otherwise = pertenece x (tail xs)

longitud :: Eq a => [a] -> Int
longitud [] = 0
longitud xs = longitud (tail xs) + 1

--de una lista string, devolver cuantas veces aparece un char
cuantasVeces :: Char -> String -> Int
cuantasVeces letra [] = 0 --nunca estuvo
cuantasVeces letra (s:xs) | letra == s = 1 + cuantasVeces letra (xs)
                          | otherwise = cuantasVeces letra (xs)

--porcentaje entre la letra, y la palabra, para ver cuanto se repite
porcentajeLetra :: Int -> Int -> Float
porcentajeLetra long letra | long > 0 = (fromIntegral letra / fromIntegral long) * 100
                           | otherwise = 0

--

-- EJ 7


frecuencia :: String -> [Float]
--Si hay alguno no mayuscula, posiciones = 0
--lista de 26 posiciones
--desde el 'a' hasta la 'z' en posiciones, devolver su porcentaje en float
--usare una funcion para contar la cantidad de una letra en una lista y sumar la cantidad
frecuencia s = frecuenciaAux s 97 

--porcentajeLetra (longitud s) (cuantasVeces x (x:xs))
--"taller" -> 97 = a ->  
--

frecuenciaAux :: String -> Int -> [Float]
frecuenciaAux s e| e <= 122 = porcentajeLetra (longitud s) (cuantasVeces (chr e) s) : frecuenciaAux s (e+1)
                 | e > 122 = []
                 | otherwise = 0 : frecuenciaAux s (e+1)


