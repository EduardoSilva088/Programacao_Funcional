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
catMaybes (h:t) case h of
	            Just a -> a : catMaybes xs
	            Nothing -> catMaybes xs

--3

data Exp a = Const a
           | Var   String
           | Mais (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)
exp1 :: Exp a
exp1 = (Mais (Var "x") (Mult (Const 3) (Const 4)))

instance Show a => Show (Exp a) where
           	show (Const a) = show a
           	show (Var a) = a
           	show (Mais a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
           	show (Mult a b) = "(" ++ show a ++ "*" ++ show b ++ ")"
