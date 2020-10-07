module Ficha3 where

--1

data Hora = H Int Int
            deriving Show

type Etapa = (Hora,Hora)

type Viagem = [Etapa]

--(a)

testaEtapa :: Etapa -> Bool
testaEtapa (H int1 int2, H int3 int4) | int3 > int1 = True
                                      | int1 == int3 && int4 > int2 = True
                                      | otherwise = False
--(b)
testaViagem :: Viagem -> Bool
testaViagem [x] = testaEtapa x
testaViagem (x:xs) | testaEtapa x == False = False
                   | otherwise = testaViagem xs

--(c)                   
calculaCP :: Viagem -> Etapa
calculaCP (h:t) = (fst h, snd $ last t)

--(d) 
calculaVE :: Viagem -> Hora
