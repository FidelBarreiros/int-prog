{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

module MisFunciones where


sonIguales :: [(String, String)] -> Bool
sonIguales [] = False
sonIguales [a] = False
sonIguales ((a,b):(a1,b1):xs) | a == b || a1 == b1 || a == a1 && b == b1 || a == b1 && b == a1 = True
                              | otherwise =  sonIguales ((a,b):xs)

relacionesValidas :: [(String, String)] -> Bool
relacionesValidas [] = True
relacionesValidas [(x, y)] |  x == y = False
                           | otherwise = True
relacionesValidas [(x, y), (x1, y1)] |  x == y || x1 == y1 || x == x1 && y == y1 || x == y1 && y == x1 = False
                                     | otherwise = True
relacionesValidas ((a,b):(a1,b1):xs) | sonIguales ((a,b):(a1,b1):xs) = False
                                     | otherwise = relacionesValidas ((a1,b1):xs)



---2-------------------------------------------

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece x (y:ys) | x == y = True
                   | otherwise = pertenece x ys


eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | pertenece x xs = eliminarRepetidos xs
                         | otherwise = x:eliminarRepetidos xs

armarLista :: [(String, String)] -> [String]
armarLista [] = []
armarLista ((a, b):xs) = a : b : armarLista xs


personas :: [(String, String)] -> [String]
personas [] = []
personas x = eliminarRepetidos(armarLista x) 


-----3---------------------------------------------------

--amigosDe :: String -> [(String, String)] -> [String]
--amigosDe _ [] = []
--amigosDe x ((a, b):xs) | pertenece x [a,b] = amigosDe x xs ++ [a,b]
--                       | otherwise = amigosDe x xs

amigosDe :: String -> [(String, String)] -> [String]
amigosDe x [] = []
amigosDe x ((a, b):xs) | x == a = b : amigosDe x xs
                       | x == b = a : amigosDe x xs
                       | otherwise = amigosDe x xs


----4----------------------------------------------------------

longitud :: (Eq t) => [t] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs


personaConMasAmigosAux :: [(String, String)] -> [String] -> String
personaConMasAmigosAux _ [x] = x 
personaConMasAmigosAux r (x:y:xs) | longitud(amigosDe x r) > longitud(amigosDe y r) = personaConMasAmigosAux r (x:xs)
                                  | otherwise = personaConMasAmigosAux r (y:xs)

personaConMasAmigos :: [(String, String)] -> String
personaConMasAmigos [] = []
personaConMasAmigos r = personaConMasAmigosAux r (personas r)


cuantasVeces :: Char -> String -> Integer
cuantasVeces letra [] = 0 --nunca estuvo
cuantasVeces letra (s:xs) | letra == s = 1 + cuantasVeces letra (xs)
                          | otherwise = cuantasVeces letra (xs)


