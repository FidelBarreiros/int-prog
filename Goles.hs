module Goles where

longitud :: (Eq t) => [t] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

longitud2 :: (Eq t) => [(t, t)] -> Integer
longitud2 [] = 0
longitud2 ((x,y):xs) = 1 + longitud2 xs

suma :: [Integer] -> Integer
suma [] = 0
suma (x:xs) = x + suma xs

contamo :: [Integer] -> Integer -> Integer
contamo [] _ = 0
contamo (x:xs) y 
                | y > suma (x:xs) = y - suma (x:xs)
                | otherwise = 0
                 


atajaronSuplentes :: [(String, String)] -> [Integer] -> Integer -> Integer
atajaronSuplentes [] _ _ = 0
atajaronSuplentes _ [] _ = 0
atajaronSuplentes _ _ 0 = 0
atajaronSuplentes ((x,y):xs) (a:as) z | longitud (a:as) == longitud2 ((x,y):xs) = contamo (a:as) z
                                      




----2-----------------------------------------------------------------------------

iguales :: [(String, String)] -> Bool
iguales [] = False
iguales ((x,y):xs) | x == y = True
                   | otherwise = iguales xs

pertenece :: (Eq t) => t -> [(t, t)] -> Bool
pertenece _ [] = False 
pertenece a ((x, y):ys) 
                        | a == x || a == y = True
                        | otherwise = pertenece a ys   

equiposValidos :: [(String, String)] -> Bool
equiposValidos [] = True

equiposValidos ((x,y):xs) 
                        | pertenece x xs || iguales ((x,y):xs) || pertenece y xs = False
                        | otherwise = equiposValidos xs


                        --"f" [("s","a"), ("a", "s")] -- "s"  [("a", "s")]


---3-----------------------------------------------------------------------------

golWacho :: String -> [(String, String)] -> [Integer] -> Float
golWacho _ [] [] = 0
golWacho a ((x, y):xs) (z:zs) | a == y = fromIntegral z
                              | otherwise =  golWacho a xs zs

porcentajeDeGoles :: String -> [(String, String)] -> [Integer] -> Float
porcentajeDeGoles a ((x, y):xs) (z:zs) =  golWacho a ((x, y):xs) (z:zs) / fromIntegral (suma (z:zs)) * 100

----4-----------------------------------------------------------------------------

vallaMenosVencida :: [(String, String)] -> [Integer] -> String
vallaMenosVencida [(a,b)] _ = b
vallaMenosVencida ((x, y):(x1, y1):xs) (z:r:zs) | z > r = vallaMenosVencida ((x1, y1):xs) (r:zs)
                                                | otherwise = vallaMenosVencida ((x, y):xs) (z:zs)
                                      