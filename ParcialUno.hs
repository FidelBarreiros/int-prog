{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use foldr" #-}
module ParcialUno where


--realidad :: [([Char], [Integer])] -> Integer -> Bool
--realidad ((a,y:ys):xs) z | mayoresAn (notasAprobadas (y:ys)) z = True
--                         | otherwise = False

notasAprobadas :: [Integer] -> Integer
notasAprobadas [] = 0
notasAprobadas (x:xs) | x >= 4 = 1 + notasAprobadas xs 
                      | otherwise = notasAprobadas xs

mayoresAn :: Integer -> Integer -> Bool
mayoresAn x z | x >= z = True
              | otherwise = False

elegirNota :: [([Char], [Integer])] -> [Char] -> [Integer]  -----Te da las notas del alumno elegido
elegirNota [] _ = []
elegirNota ((a,y:ys):xs) b | b == a = y:ys
                       | otherwise = elegirNota xs b

aproboMasDeNMaterias :: [([Char], [Integer])] -> [Char] -> Integer -> Bool
aproboMasDeNMaterias [] _ _ = False
aproboMasDeNMaterias ((a,(y:ys)):xs) x z | mayoresAn (notasAprobadas (elegirNota ((a,(y:ys)):xs) x)) z = True
                                           | otherwise = False


mayores4 :: [Integer] -> [Integer]
mayores4 [] = []
mayores4 (x:xs) | x >= 4 = x:mayores4 xs
                | otherwise = mayores4 xs

cuantosMayor4 :: [Integer] -> Integer
cuantosMayor4 [] = 0
cuantosMayor4 (x:xs) = 1 + cuantosMayor4 xs

--- nota [1,6,8,3,8] = [6,8,8], luego, que superen a n, ponele que n=2
aproboMasDeXMaterias :: [([Char],[Integer])] -> [Char] -> Integer -> Bool
aproboMasDeXMaterias ((a,b):xs) alumno n = n > cuantosMayor4 (mayores4 b)
--2----------------------------------------------------------------------------------

longitud :: [Integer] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

suma :: [Integer] -> Integer -----te da la suma de las notas
suma [] = 0
suma (x:xs) =  x +  (suma (xs))


buenosAlumnos :: [([Char], [Integer])] -> [[Char]]
buenosAlumnos [] = []
buenosAlumnos ((a,(y:ys)):xs) | notasAprobadas (y:ys) == longitud (y:ys) && promedio (suma(y:ys)) (longitud(y:ys)) >= 8 = a:buenosAlumnos xs
                              | otherwise = buenosAlumnos xs


promedio :: Integer -> Integer -> Float
promedio 0 1 = 0
promedio x y = fromInteger x / fromInteger y


----3----------------------------------------------------------------------------------


mejorPromedio :: [([Char], [Integer])] -> [Char]    --te da el nombre del alumno con mejor promedio
mejorPromedio [] = []
mejorPromedio [(nombre, calificaciones)] = nombre
mejorPromedio ((a,(y:ys)):(b,(t:ts)):xs) | promedio (suma(y:ys)) (longitud(y:ys)) >= promedio (suma(t:ts)) (longitud(t:ts)) = mejorPromedio ((a,(y:ys)):xs)
                                         | otherwise = mejorPromedio ((b,(t:ts)):xs)



-------4-----------------------------------------------------------------------------

mejorPromedioNota :: [([Char], [Integer])] -> Float   --es como la de mejor promedio pero te da la calificacion en vez del nombre
mejorPromedioNota [] = 0
mejorPromedioNota [(nombre, calificaciones)] = promedio (suma(calificaciones)) (longitud(calificaciones))
mejorPromedioNota ((a,(y:ys)):(b,(t:ts)):xs) | promedio (suma(y:ys)) (longitud(y:ys)) >= promedio (suma(t:ts)) (longitud(t:ts)) = mejorPromedioNota ((a,(y:ys)):xs) 
                                             | otherwise = mejorPromedioNota ((b,(t:ts)):xs)


pertenece :: [Char] -> [[Char]] -> Bool --para preguntar si el alumno pertenece a la lista de buenos alumnos
pertenece t [] = False
pertenece t (x:xs) | t == x = True
                   | otherwise = pertenece t xs


seGraduoConHonores :: [([Char], [Integer])] -> Integer -> [Char] -> Bool
seGraduoConHonores ((a,(y:ys)):xs) x z | aproboMasDeNMaterias ((a, (y : ys)) : xs) z (x - 1) &&
                                         pertenece z (buenosAlumnos ((a,(y:ys)):xs)) &&
                                         promedio (suma (elegirNota ((a, (y : ys)) : xs) z)) (longitud (elegirNota((a, (y : ys)) : xs) z)) > mejorPromedioNota ((a,(y:ys)):xs) - 1 = True
                                       | otherwise = False


