{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Codificar where
-- * --- --- --- --- EJERCICIO 1: Hay que Codificar --- --- --- --- * --
hayQueCodificar :: Char -> [(Char, Char)] -> Bool
hayQueCodificar _ [] = False
hayQueCodificar caracter ((a, _):xs) = caracter == a || hayQueCodificar caracter xs

-- * --- --- --- --- EJERCICIO 2: Cuantas Veces Hay Que Codificar --- --- --- --- * --
cuantasVecesHayQueCodificar :: Char -> [Char] -> [(Char, Char)] -> Int
cuantasVecesHayQueCodificar caracter frase mapeo | not (hayQueCodificar caracter mapeo) = 0
                                                 | otherwise = cantidadApariciones caracter frase

cantidadApariciones :: Char -> [Char] -> Int -- No hace falta hacer esta funcion
cantidadApariciones _ [] = 0  
cantidadApariciones caracter (f:fs) | caracter == f = 1 + cantidadApariciones caracter fs
                                    | otherwise = cantidadApariciones caracter fs

-- * --- --- --- --- EJERCICIO 3: La Que Mas Hay Que codificar --- --- --- --- * --
laQueMasHayQueCodificar :: [Char] -> [(Char, Char)] -> Char
laQueMasHayQueCodificar frase mapeo = laQueMasHayQueCodificarAux frase frase mapeo

laQueMasHayQueCodificarAux :: [Char] -> [Char] -> [(Char, Char)] -> Char
laQueMasHayQueCodificarAux [f] _ _ = f
laQueMasHayQueCodificarAux (f1:f2:fs) fraseOriginal mapeo 
    | f1 == f2 = laQueMasHayQueCodificarAux (f1:fs) fraseOriginal mapeo -- Saco repetido
    | cuantasVecesHayQueCodificar f1 fraseOriginal mapeo >= cuantasVecesHayQueCodificar f2 fraseOriginal mapeo = laQueMasHayQueCodificarAux (f1:fs) fraseOriginal mapeo
    | otherwise = laQueMasHayQueCodificarAux (f2:fs) fraseOriginal mapeo


-- * --- --- --- --- EJERCICIO 4: Codificar Frase --- --- --- --- * --
codificarFrase :: [Char] -> [(Char, Char)] -> [Char]
codificarFrase [] _ = []
codificarFrase (f:fs) mapeo | hayQueCodificar f mapeo = obtenerReemplazo f mapeo : codificarFrase fs mapeo
                            | otherwise = f : codificarFrase fs mapeo

obtenerReemplazo :: Char -> [(Char, Char)] -> Char
obtenerReemplazo caracter ((a, reemplazo):xs) | caracter == a = reemplazo
                                              | otherwise = obtenerReemplazo caracter xs





