import Distribution.Simple.Setup (falseArg)
import Distribution.TestSuite (Test(Test))
doubleMe :: Int -> Int
doubleMe x = x + x

sumarUno :: Int -> Int
sumarUno x = x + 1

f :: Int -> Int
f n | n == 1 = 8
    | n == 4 = 131
    | n == 16 = 16

g :: Int -> Int
g n | n == 8 = 16
    | n == 16 = 4
    | n == 131 = 1

h :: Int -> Int
h x = f (g x)

k :: Int -> Int
k x = g (f x)




absoluto :: Int -> Int
absoluto n | n >= 0 = n
           | otherwise = -n

maxabs :: Int -> Int -> Int 
maxabs x y | x < 0 = -x
           | y < 0 = -y
           | x > y = x
           | otherwise = y

max3 :: Int -> Int -> Int -> Int
max3 x y z | x > y && x > z = x
          | y > x && y > z = y
          | z > x && z > y = z

alges :: Float -> Float -> String 
alges x y | x == 0 || y == 0 = "si"
          | otherwise = "no"


alges2 :: Float -> Float -> String 
alges2 x 0 = "si"
alges2 0 y = "si"
alges2 x y = "no"

ambos :: Float -> Float -> String 
ambos x y | x == 0 && y == 0 = "si"
          | otherwise = "no"

ambos2 :: Float -> Float -> String 
ambos2 0 0 = "si"
ambos2 x y = "no"


mismo :: Float -> Float -> String
mismo x y | x <= 3 && y <= 3 = "si"
          | (x > 3 && x <= 7) && (y > 3 && y <= 7) = "si"
          | x > 7 && y > 7 = "si"
          | otherwise = "no"

sumad :: Int -> Int -> Int -> Int
sumad x y z | x /= y && x /= z && y /= z = x + y + z

multip :: Int -> Int -> String
multip x y | mod x y == 0 = "es multiplo"
           | otherwise = "no es multiplo"
            
digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10

digdec :: Int -> Int
digdec x = mod x 100

--ej 3--

eRel :: Int -> Int -> Bool
eRel a b | a == 0 && b == 0 = False
         | mod a b == 0 = True
         | otherwise = False

{-ej 4-}

prodInt :: (Int, Int) -> (Int, Int) -> (Int, Int)
prodInt (ax, ay) (bx, by) = (ax * bx, ay * by)

{-ej 5-}

todMen :: (Int, Int, Int) -> Bool
todMen (x, y, z) | f2 x > g2 x && f2 y > g2 y && f2 z > g2 z = True
                 | otherwise = False

f2 :: Int -> Int
f2 x | x <= 7 = x^2
     | otherwise = 2 * x - 1

g2 :: Int -> Int
g2 x | even x = div x 2
     | otherwise = 3 * x + 1

dMan :: (Float, Float, Float) -> (Float, Float, Float) -> Float
dMan (ax, ay, az) (bx, by, bz) = abs((ax - bx) + (ay - by) + (az - bz)) 