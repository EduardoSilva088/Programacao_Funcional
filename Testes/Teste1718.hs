module Teste1718 where 

import System.Random

--1

insert' :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert' x (h:t) | x <= h = (x:h:t)
                | otherwise = x : insert' x t

--2
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (h:t) = case h of Just x -> x:catMaybes t
                            Nothing -> catMaybes t

--3

data Exp a = Const a
           | Var   String
           | Mais (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

instance Show a => Show (Exp a) where
            show (Const a) = show a
            show (Var a) = a
            show (Mais a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
            show (Mult a b) = "(" ++ show a ++ " * " ++ show b ++ ")"           

--4
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f [] = []
sortOn f (h:t) = insert' h (sortOn f t)
               where insert' x [] = [x]
                     insert' x (h:t) | f x <= f h = (x:h:t)
                                     | otherwise = h : insert' x t

--5

--a
amplitude :: [Int] -> Int
amplitude [] = 0
amplitude l = max l - min l
            where max [x] = x
                  max [] = 0
                  max (h:y:t) | h > y = max (h:t)
                              | otherwise = max (y:t)
                  min [x] = x
                  min [] = 0
                  min (h:y:t) | h > y = min (y:t)
                              | otherwise = min (h:t)

--b                              
parte :: [Int] -> ([Int],[Int])
parte = undefined

--5

data Imagem = Quadrado Int
              | Mover (Int,Int) Imagem
              | Juntar [Imagem]


ex :: Imagem
ex = Mover (5,5) (Juntar [Mover (0,1) (Quadrado 5),
                          Quadrado 4,
                          Mover (4,3) (Quadrado 2)])


--(a)
conta :: Imagem -> Int
conta (Quadrado _) = 1
conta (Mover (_,_) i) = conta i
conta (Juntar li) = sum (map conta li)

--(b)
apaga :: Imagem -> IO Imagem
apaga i = do
         let indQuadrado = indiceQuadrados i
         randomNum <- randomRIO (1, length indQuadrado)
         let indtoRemove = indQuadrado !! (randomNum -1)
         return $ apagaindice indtoRemove i

indiceQuadrados :: Imagem -> [Int]
indiceQuadrados (Quadrado n) = [n]
indiceQuadrados (Mover (_,_) i) = indiceQuadrados i
indiceQuadrados (Juntar li) = concatMap indiceQuadrados li

apagaindice :: Int -> Imagem -> Imagem
apagaindice x (Quadrado n) | x == n = Juntar []
                           | otherwise = (Quadrado n)
apagaindice x (Mover (a,b) i) = Mover (a,b) (apagaindice x i)
apagaindice x (Juntar li) = Juntar (map (apagaindice x) li)






