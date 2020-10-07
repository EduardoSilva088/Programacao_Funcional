module Ficha where
import Data.Char

--2

--(a)
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = h*2 : dobros t

--(b)
numOcorre :: Char -> String -> Int
numOcorre _ [] = 0
numOcorre x (h:t) = if x == h then 1 + numOcorre x t
                    else numOcorre x t 

--(c)
positivos :: [Int] -> Bool 
positivos [] = False
positivos [x] = if x > 0 then True else False
positivos (h:t) = if h > 0 then positivos t else False

--(d)
soPos :: [Int] -> [Int]
soPos [] = []
soPos [x] = if x > 0 then [x]
            else []
soPos (x:xs) = if x > 0 then soPos xs 
               else soPos xs

--(e)
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg [x] = if x > 0 then 0 else x
somaNeg (x:xs) = if x < 0 then x + somaNeg xs
                 else somaNeg xs

--(f)
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (x:xs) = if length (x:xs) < 3 then (x:xs)
                 else tresUlt xs

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos (x:xs) = snd x : segundos xs

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros x (y:ys) | x == fst y = True
                      | otherwise = nosPrimeiros x ys

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((x,y,z):xs) = soma (x,y,z) (sumTriplos xs)
                    where soma (x,y,z) (xs,ys,zs) = (x+xs,y+ys,z+zs)

--3

--(a)
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (x:xs) | isDigit x = x : soDigitos xs
                 | otherwise = soDigitos xs

--(b)
minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (x:xs) | isLower x = 1 + minusculas xs
                  | otherwise = minusculas xs

nums :: String -> [Int]
nums [] = [0]
nums (x:xs) | isDigit x = digitToInt x : nums xs
            | otherwise = nums xs

type Polinomio = [Monomio]
type Monomio = (Float,Int)

--4
p :: Polinomio 
p = [(2,3), (3,4), (5,3), (4,5)]

p' :: Polinomio
p' = [(0,1),(1,2),(2,3)]

--(a)
conta :: Int -> Polinomio -> Int
conta n [] = 0
conta n (x:xs) | n == snd x = 1 + conta n xs
               | otherwise = conta n xs

grau :: Polinomio -> Int
grau [] = 0
grau [(b,g)] = g
grau ((b,g):(b1,g1):xs) | g > g1 = grau ((b,g):xs)
                        | otherwise = grau ((b1,g1):xs)

selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau n [(b,g)] | n == g = [(b,g)]
selgrau n (x:xs) | n == snd x = x : selgrau n xs
                 | otherwise = selgrau n xs                     

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv [(_,0)] = []
deriv ((b,g):xs) = (b* fromIntegral g,g-1) : deriv xs

calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula x ((b,g):xs) = (x * b)^fromIntegral g + calcula x xs

simp :: Polinomio -> Polinomio
simp [] = []
simp (x:xs) | fst x == 0 = simp xs
            | otherwise = x : simp xs

mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []            
mult (b,g) ((b1,g1):xs) = (b*b1,g+g1) : mult (b,g) xs

normaliza :: Polinomio -> Polinomio
normaliza = undefined 








