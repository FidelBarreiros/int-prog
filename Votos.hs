{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Data.Traversable (for)
{-# HLINT ignore "Use foldr" #-}


suma :: [Integer] -> Integer
suma [] = 0
suma (x:xs) = x + suma xs

votosEnBlanco :: [(String, String)] -> [Integer] -> Integer -> Integer
votosEnBlanco [] [] x = x
votosEnBlanco ((x,y):xs) (t:ts) total = total - suma (t:ts) 

----2-----------------------------------------------------------------------------------

pertenece :: (Eq t) => t -> [(t, t)] -> Integer
pertenece _ [] = 0
pertenece f ((x,y):xs) | f == x || f == y = 1 + pertenece f xs
                       | otherwise = pertenece f xs


formulasValidas :: [(String, String)] -> Bool
formulasValidas [] = True
formulasValidas ((x,y):xs) | (pertenece x ((x,y):xs) > 1 || pertenece y ((x,y):xs) > 1) || x == y  = False
                           | otherwise = formulasValidas xs

----3--------------------------------------------------------------------------------------

division :: Integer -> Integer -> Float
division a b = (fromIntegral a) / (fromIntegral b)

elegir :: String -> [(String, String)] -> [Integer] -> Integer
elegir _ _ [] = 0
elegir presi ((x,y):xs) (t:ts) | presi == x = t
                               | otherwise = elegir presi xs ts

porcenteajeDeVotos :: String -> [(String, String)] -> [Integer] -> Float
porcenteajeDeVotos presi ((x,y):xs) (t:ts) = (division (elegir presi ((x,y):xs) (t:ts))  (suma (t:ts))) * 100

-------4---------------------------------------------------------------------------------------


elegirMayor :: [Integer] -> Integer
elegirMayor [] = 0
elegirMayor [x] = x
elegirMayor (x:y:xs) | x >= y = elegirMayor (x:xs)
                     | otherwise = elegirMayor (y:xs)

elegirPresi :: [(String, String)] -> [Integer] -> [Integer] -> String

elegirPresi  ((x,y):xs) (t:ts) (f:fs) | elegirMayor (t:ts) == f = x
                                      | otherwise = elegirPresi xs (t:ts) fs

proximoPresidente :: [(String, String)] -> [Integer] -> String
proximoPresidente ((x,y):xs) (t:ts) = elegirPresi ((x,y):xs) (t:ts) (t:ts)
