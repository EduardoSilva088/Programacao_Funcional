module Ficha8 where

import Data.List


--Introdução!

data Cor = Verde    Int
         | Vermelho Int
         | Azul     Int

a :: Cor 
a = Azul 60


v :: Cor
v = Verde 20

cores :: [Cor]
cores = [a,Vermelho 19,v,Azul 90]

showCor :: Cor -> String
showCor (Verde i) | i < 20 = "Verde Claro"
                  | i > 80 = "Verde Escuro"
                  | otherwise = "Verde"

showCor (Vermelho i) | i < 20 = "Vermelho Claro"
                     | i > 80 = "Vermelho Escuro"
                     | otherwise = "Vermelho"

showCor (Azul i)  | i < 20 = "Azul Claro"
                  | i > 80 = "Azul Escuro"
                  | otherwise = "Azul"

instance Show Cor where
                show = showCor

corIgual :: Cor -> Cor -> Bool
corIgual (Vermelho _) (Vermelho _) = True
corIgual (Azul _) (Azul _)         = True
corIgual (Verde _) (Verde _)       = True
corIgual _ _                       = False

instance Eq Cor where
                (==) = corIgual

comparaCor :: Cor -> Cor -> Bool
comparaCor (Verde i) (Vermelho j) = i > j
comparaCor (Verde i) (Azul j)     = i > j
comparaCor (Verde i) (Verde j)    = i > j
comparaCor (Vermelho i) (Azul j)  = i > j
comparaCor (Vermelho i) (Verde j) = i > j
comparaCor (Vermelho i) (Vermelho j) = i > j
comparaCor (Azul i) (Vermelho j)     = i > j
comparaCor (Azul i) (Verde j)        = i > j
comparaCor (Azul i) (Azul j)         = i > j

instance Ord Cor where
                (<=) a b = comparaCor b a


--Ficha 8

data Frac = F Integer Integer

data (Exp a) = Const a 
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

-- infixa
e :: Exp Int
e = (Const 3) `Mais` ((Const 4) `Mult` (Const 5))

--prefixa

e' :: Exp Int
e' = Mais (Const 3) (Mult (Const 4) (Const 5))

--(a)
calcula :: Num a => Exp a -> a
calcula (Const c)     = c
calcula (Simetrico e) = - (calcula e)
calcula (Mais e d)    = calcula e + calcula d
calcula (Menos e d)   = calcula e - calcula d
calcula (Mult e d)    = calcula e * calcula d

--(b)
infixa :: Show a => Exp a -> String
infixa (Const c)     = show c
infixa (Simetrico e) = "( -" ++ infixa e ++ ")"
infixa (Mais e d)    = "(" ++ infixa e ++ "+" ++ infixa d ++ ")"
infixa (Menos e d)   = "(" ++ infixa e ++ "-" ++ infixa d ++ ")"
infixa (Mult e d)    = "(" ++ infixa e ++ "*" ++ infixa d ++ ")"

--(c)
posfixa :: Show a => Exp a -> String
posfixa (Const c)      = show c
posfixa (Simetrico e)  = "-" ++ posfixa e
posfixa (Mais e d)     = posfixa e ++ " " ++ posfixa d ++ " +"
posfixa (Menos e d)    = posfixa e ++ " " ++ posfixa d ++ " -"
posfixa (Mult e d)     = posfixa e ++ " " ++ posfixa d ++ " *"

instance Show a => Show (Exp a) where
     show = infixa

comparaExp :: (Num a,Eq a) => Exp a -> Exp a -> Bool
comparaExp e1 e2 = calcula e1 == calcula e2

instance (Num a, Eq a) => Eq (Exp a) where
     (==) = comparaExp


somaExp :: Exp a -> Exp a -> Exp a
somaExp e1 e2 = Const (calcula e1 + calcula e2)

absExp :: Exp a -> Exp a
absExp e = Const (abs (calcula e))

fromIntegerExp :: Num a => Integer -> Exp a
fromIntegerExp i = Const (fromInteger i)

instance Num a => Num (Exp a) where
       (+)         = somaExp
       (*)         = mulExp
       (-)         = menosExp
       abs         = absExp
       signum      = signumExp
       fromInteger = fromIntegerExp

--(a) Não tem a ver com classes.


--(b) Defina Frac como instancia da classe Eq.


--(c) Defina Frac como instancia da classe Ord.



--(d) Defina Frac como instancia da classe Show, de forma a que cada fracao seja apresentada por (numerador/denominador).




