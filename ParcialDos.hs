{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant ==" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module ParcialDos where

hayQueCodificar :: Char -> [(Char, Char)] -> Bool
hayQueCodificar _ [] = False
hayQueCodificar c ((x,y):xs) | c == x = True
                             | otherwise = hayQueCodificar c xs

---2--------------------------------------------------------------------------------------

contar :: Char -> [Char] -> Integer
contar _ [] = 0
contar c (x:xs) | c == x = 1 + contar c xs
                | otherwise = contar c xs

cuantasVecesHayQueCodificar :: Char -> [Char] -> [(Char, Char)] -> Integer
cuantasVecesHayQueCodificar c (f:fs) ((x,y):xs) | hayQueCodificar c ((x,y):xs) == False = 0
                                                | otherwise = contar c (f:fs)
                                            

--3-----------------------------------------------------------------


laQueMasHayQueCodificar :: [Char] -> [(Char, Char)] -> [Char]
laQueMasHayQueCodificar frase mapeo = laQueMasHayQueCodificarAux frase frase mapeo

laQueMasHayQueCodificarAux :: [Char] -> [Char] -> [(Char, Char)] -> [Char]
laQueMasHayQueCodificarAux [x] _ _ = [x]
laQueMasHayQueCodificarAux (f:r:fs) (t:u:ts) ((x,y):xs) | cuantasVecesHayQueCodificar f (t:u:ts) ((x,y):xs) >= cuantasVecesHayQueCodificar r (t:u:ts) ((x,y):xs) = laQueMasHayQueCodificarAux (f:fs) (t:u:ts) ((x,y):xs)
                                            | otherwise = laQueMasHayQueCodificarAux (r:fs) (t:u:ts) ((x,y):xs)

----4----------------------------------------------------------------


reemplazar :: Char -> [(Char, Char)] -> Char
reemplazar x ((y,f):ys) | x == y = f
                        | otherwise = reemplazar x ys


codificarFrase :: [Char] -> [(Char, Char)] -> [Char]
codificarFrase [] _ = []
codificarFrase (f:fs) ((x,y):xs) | hayQueCodificar f ((x,y):xs) = reemplazar f ((x,y):xs) : codificarFrase fs ((x,y):xs)
                                 | otherwise = f : codificarFrase fs ((x,y):xs)  