module Ficha2 where
import Data.Char

--1

--(a)
funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys)

{-funA [2,3,4,1]
= 2^2 + funA [3,4,1]
= 4 + 3^2 +5^2 + 1^2
= 39 -}


--(b)
funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2) == 0 then h : (funB t)
                                        else (funB t)

{- funB [8,5,12]
= [8,12] -}



--(c)
{-funC (x:y:t) = funC t
funC [x] = []
funC [] = []

{- funC [1,2,3,4,5]
= funC [3,4,5]
= funC [5]
= [] -}

--(d)
funD l = g [] l
g l [] = l
g l (h:t) = h (h:1)    -}



--2

--(a)
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = (h*2) : dobros t

--(b)     (String = [Char])
numOcorre :: Char -> String -> Int
numOcorre _ [] = 0
numOcorre a (h:t) | a == h = 1 + numOcorre a t
                  | otherwise = numOcorre a t

--(c)
positivos :: [Int] -> Bool
positivos [x] | x > 0 = True
              | otherwise = False
positivos (x:xs) = if x > 0 then positivos xs else False

--(d)
soPos :: [Int] -> [Int]
soPos [x] = if x > 0 then [x] else []
soPos (x:xs) = if x > 0 then x : soPos xs else soPos xs

--(e)
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg [x] = if x < 0 then x else 0
somaNeg (x:xs) = if x < 0 then (x + somaNeg xs) else somaNeg xs

--(f)
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (x:xs) = if length xs <= 2 then x:xs
                                   else tresUlt xs

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos (x:xs) = snd x : segundos xs

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros a [] = False
nosPrimeiros a (x:xs) = if a == fst x then True else nosPrimeiros a xs

{-sumTriplos :: (Num a ,Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos (x:xs) = fst x -}


{-
isDigit :: Char -> Bool
> isDigit 'a' -> False
> isDigit '9' -> True

isAlpha :: Char -> Bool
isLower :: Char -> Bool
'a' -> True
'D' -> False
-}

--3

--(a)
soDigitos :: [Char] -> [Char]
soDigitos (x:xs) | isDigit x = x : soDigitos xs
                 | otherwise = soDigitos xs

--(b)
minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (x:xs) | isLower x = 1 + minusculas xs
                  | otherwise = minusculas xs

--(c)
nums :: [Char] -> [Int]
nums [] = []
nums (x:xs) | isDigit x = digitToInt x : nums xs
            | otherwise = nums 


--4
type Polinomio = [Monomio]
type Monomio = (Float,Int)

p :: Polinomio
p = [(2,3), (3,4), (5,3), (4,5)]
--a
conta :: Int -> Polinomio -> Int
conta n p = if n == snd (head p) then 1 + conta (n (tail p)) else 0





