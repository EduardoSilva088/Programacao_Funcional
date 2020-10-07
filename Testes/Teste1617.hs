module Teste1617 where 

--1

type MSet a = [(a,Int)]

ex1 :: MSet Char
ex1 = [('b',4),('a',3),('c',2),('d',1)]

--(a)
cardMSet :: MSet a -> Int
cardMSet [] = 0
cardMSet ((x,n):xs) = n + cardMSet xs

--(b)
moda :: MSet a -> [a]
moda = undefined


--(c)
converteMSet :: MSet a -> [a]
converteMSet [] = []
converteMSet ((x,n):xs) = aux (x,n) ++ converteMSet xs
                         where aux (x,n) = replicate n x


--(d)
addNcopies :: Eq a => MSet a -> a -> Int -> MSet a 
addNcopies a@((x,n):xs) x1 n1 | n1 >= n = ((x1,n1):a)
                              | n1 < n = (x,n) : addNcopies xs x1 n1

--2

data SReais = AA Double Double | FF Double Double
            | AF Double Double | FA Double Double
            | Uniao SReais SReais

ex2 :: SReais
ex2 = Uniao (Uniao (AA 4.2 5.5) (AF 3.1 7.0)) (FF (-12.3) 30.0)

--(a)
instance Show SReais where
                show = showSReais

showSReais (Uniao a b) = "(" ++ show a ++ "U" ++ show b ++ ")"
showSReais (AA a b) = "]" ++ show a ++ "," ++ show b ++ "["
showSReais (FF a b) = "[" ++ show a ++ "," ++ show b ++ "]"
showSReais (AF a b) = "]" ++ show a ++ "," ++ show b ++ "]"
showSReais (FA a b) = "[" ++ show a ++ "," ++ show b ++ "["

--(b)
pertence :: Double -> SReais -> Bool
pertence x (Uniao a b) = pertence x a || pertence x b
pertence x (AA a b) = if x > a && x < b then True else False
pertence x (FF a b) = if x >= a && x <= b then True else False
pertence x (AF a b) = if x > a && x <= b then True else False
pertence x (FA a b) = if x >= a && x < b then True else False

--(c)
tira :: Double -> SReais -> SReais
tira = undefined



--3

data RTree a = R a [RTree a]

rt :: RTree Int
rt = R 5 [ R 2 [ R 3 []
               ]
         , R 7 []
         , R 4 [ R 11 [ R 13 []
                      ]
               , R 8 []
               ]
         ]
--(a)
percorre :: [Int] -> RTree a -> Maybe [a]
percorre = undefined

--(b)
procura :: Eq a => a -> RTree a -> Maybe [Int]
procura = undefined





