module Ficha5 where
import Data.List

--1

--(a)
any' :: (a -> Bool) -> [a] -> Bool
any' p [] = False
any' p (x:xs) = p x || any' p xs

--(b)
zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


--(c)
takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs
                    | otherwise = []

--(d)
dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = x : xs

--(e)
span' :: (a-> Bool) -> [a] -> ([a],[a])
span' p l = (takeWhile' p l, dropWhile' p l)

-- span'' :: (a-> Bool) -> [a] -> ([a],[a])
-- span'' p (x:xs) | p x = (x : span'' p xs, span'' p xs)
--                 | otherwise = ([], (x:xs))


-- (f) 
deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy = undefined

--(g)
sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f [] = []
sortOn' f (h:t) = myinsert h (sortOn' f t)
                where myinsert x [] = [x]
                      myinsert x (y:ys) | f x <= f y = x:y:ys
                                        | otherwise = y : myinsert x ys



--2

type Polinomio = [Monomio]

type Monomio = (Float,Int)

p :: Polinomio
p = [(2,3), (3,4), (5,3), (4,5)]

p1 :: Polinomio
p1 = [(1,2),(2,4),(1,3)]

--(a)
selgrau :: Int -> Polinomio -> Polinomio
selgrau n p = filter f p
            where f (b,g) = g == n

--(b)
conta :: Int -> Polinomio -> Int
conta n p = length (filter (\(c,g) -> g == n) p)

--(c)
grau :: Polinomio -> Int
grau p = maximum (map snd p)           

--(d)
derivmonomio :: Monomio -> Monomio
derivmonomio (b,g) = (b * fromIntegral g, g-1)

deriv :: Polinomio -> Polinomio
deriv pol = map derivmonomio pol

--(e)
calcula :: Float -> Polinomio -> Float
calcula x pol = sum (map (\(b,g) -> b * x^g) pol)

--(f)
simp :: Polinomio -> Polinomio
simp pol = filter (\(b,g) -> g /= 0) pol

--(g)
mult :: Monomio -> Polinomio -> Polinomio
mult (b,g) pol = map (\(b1,g1) -> (b*b1, g+g1)) pol

--(h)
ordena :: Polinomio -> Polinomio
ordena pol = sortOn snd pol

--(i)
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza (h:t) = acrex h (normaliza t)

acrex :: Monomio -> Polinomio -> Polinomio
acrex mon [] = [mon]
acrex (b,g) ((b1,g1):t) | g == g1 = (b+b1, g1):t
                        | otherwise = (b1,g1) : acrex (b,g) t

--(j)
soma :: Polinomio -> Polinomio -> Polinomio
soma [] p = p
soma p [] = p
soma pol1 pol = normaliza (pol1 ++ pol)

--(k)
produto :: Polinomio -> Polinomio -> Polinomio
produto p [] = []
produto [] p = []
produto (h:t) pol = soma (mult h pol) (produto t pol)

--(l)
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = normaliza p1 == normaliza p2

--3

type Mat a = [[a]]

-- m1 :: Mat a
-- m1 = [[1,2,3], [0,4,5], [0,0,6]]

--(a)
dimOK :: Mat a -> Bool
dimOK [] = True
dimOK [x] = True
dimOK (x:y:xs) | length x /= length y = False
               | otherwise = dimOK (x:xs)

--(b)
dimMat :: Mat a -> (Int,Int)
dimMat [] = (0,0)
dimMat m1@(x:xs) = (length m1, length x)

--(c)
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat [] [] = []
addMat matriz [] = matriz
addMat [] matriz = matriz
addMat m1 m2 = zipWith' (zipWith' (+)) m1 m2

--(d)
transpose' :: Mat a -> Mat a
transpose' [] = []
transpose' mat | length (head mat) > 1 = (map head mat) : (transpose' (map tail mat))
               | otherwise =  [(map head mat)]

--(e)
multMat :: Num a => Mat a -> Mat a -> Mat a
multMat = undefined               

--(f)
zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat f [] m = m
zipWMat f m [] = m
zipWMat f m1 m2 = zipWith (zipWith f) m1 m2

--(g)
triSup :: (Eq a,Num a) => Mat a -> Bool
triSup [] = False
triSup mat = tAux 0 mat 
           where  tAux x [] = True
                  tAux x (h:ts) = if ((length (fst (span (0 ==) h))) >= x) then tAux (x + 1) ts else False

--(h)
rotateLeft :: Mat a -> Mat
rotateLeft [] = []
rotateLeft m | length (head m) > 1 = rotateLeft (map (drop 1) m) ++ [map head m]
             | otherwise = [map head m]


