module Aula1 where

--1

--(a)
perimetro r = 2*pi*r

--(b)
dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

--(c)
primUlt :: [a] -> (a,a)
primUlt l = (head l, last l)

--(d)
multiplo :: Int -> Int -> Bool
multiplo m n = if (mod m n)== 0 then True else False
-- ou
multiplo2 m n = mod m n == 0

--(e)
truncaImpar lista = if (mod (length lista) 2)== 0 then lista else tail lista

--(f)
max2 :: Int -> Int -> Int
max2 x y = if (x>y) then x else y

--(g)
max3 :: Int -> Int -> Int -> Int
max3 x y z = if (max2 x y >z) then max2 x y else z

--2

--(a)
nRaizes :: Double -> Double -> Double -> Int
nRaizes a b c = if a==0 then 1
              else if (b^2 -4*a*c)<0 then 0
              else if (b^2 -4*a*c)==0 then 1
              else 2

--(ou)
nRaizes1 a b c
     |a==0 = 1
     |delta < 0 = 0
     |delta > 0 = 1
     |otherwise = 2
      where delta = b^2 -4*a*c

--(b)
raizes :: Double -> Double -> Double -> [Double]
raizes a b c 
     |(delta < 0) = [ ]
     |(a == 0) = [r3]
     |delta == 0 = [r]
     |otherwise = [r1,r2]
      where delta = (b^2 -4*a*c)
            r = (-b)/(2*a)
            r1 = ((-b + sqrt (b^2 -4*a*c))/2*a)    
            r2 = ((-b - sqrt (b^2 -4+a+c))/2*a)
            r3 = ((-c)/b)
--3

type Hora = (Int, Int)

h1 :: Hora
h1 = (0,15)

h2 :: Hora
h2 = (13,45)

--(a)
valida :: Hora -> Bool
valida (h,m) = h >= 0 && h < 24 && m >= 0 && m < 60
--(b)

comparacao :: Hora -> Hora -> Bool
comparacao (h1,m1) (h2,m2) = if h1> h2 then True
                           else if h1 < h2 then False
                           else if h1 == h2 && m1 > m2 then True
                           else False

--(c)

converterH :: Hora -> Int
converterH (h,m) = 60*h + m

--(d)

converterM :: Int -> Hora
converterM x = (div x 60, mod x 60)

--(e)

diferenca :: Hora -> Hora -> Int
diferenca (h1,m1) (h2,m2) = (h1-h2)*60 + (m1-m2)

--(f)

somar :: Hora -> Int -> Hora
somar (h,m) x = if valida (h1,m1) == True && x == 0 then (h1,m1)
             else if h1 > 24 && m1 > 60 then ((mod h1 24) + (div m1 60),(mod m1 60))
             else if h1 > 24 && m1 == 60 then ((mod h1 24) + 1,00)
             else if h1 > 24 && m1 < 60 then ((mod h1 24),m1)
             else if h1 == 24 && m1 > 60 then (h1 + (div m1 60),(mod m1 60))
             else if h1 == 24 && m1 == 60 then (h1 + 1,00)
             else if h1 == 24 && m1 < 60 then (h1,m1)
             else if h1 < 24 && m1 > 60 then (h1 + (div m1 60),(mod m1 60))
             else if h1 < 24 && m1 == 60 then
                           if h1 + 1 == 24 then (0,0)
                           else (h1 + 1,0)
             else (h1,m1)
             where h1 = h + (div x 60)
                   m1 = m + (mod x 60)

--4

data Hhora = H Int Int deriving (Show,Eq)

--(a)

validaT :: Hhora -> Bool
validaT (H h m) = (elem h [0..23]) && (elem m [0..59])

--(b)

comparacaoT :: Hhora -> Hhora -> Bool
comparacaoT (H h m) (H h1 m1) = if h>h1 then True
                                 else if h < h1 then False
                                 else if h == h1 && m > m1 then True
                                 else False

--(c)

converterHT :: Hhora -> Int
converterHT (H h m) = h*60 + m

--(d)

converterMT :: Int -> Hhora
converterMT x = H (div x 60) (mod x 60)


--(e)

diferencaT :: Hhora -> Hhora -> Int
diferencaT (H h m) (H h1 m1) = (h-h1)*60 + (m-m1)
--(f)




--5

data Semaforo = Verde 
              | Amarelo
              | Vermelho
              deriving (Show,Eq)

s1 :: Semaforo
s1 = Verde

s2 :: Semaforo
s2 = Amarelo

--(a)

nextSemaforo :: Semaforo -> Semaforo
nextSemaforo Verde = Amarelo
nextSemaforo Amarelo = Vermelho
nextSemaforo Vermelho = Verde



--(b)

stopSemaforo :: Semaforo -> Bool
stopSemaforo Vermelho = True
stopSemaforo _ = False

--(c) 

seguroSemaforo :: Semaforo -> Semaforo -> Bool
seguroSemaforo Vermelho _ = True
seguroSemaforo _ Vermelho = True
seguroSemaforo _ _ = False

--6

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

--(a)
posx :: Ponto -> Double
posx (Cartesiano x y) = x

--(b)
posy :: Ponto -> Double
posy (Cartesiano x y) = y

--(c)
raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt(x^2 + y^2)

--(d)
angulo :: Ponto -> Double
angulo (Cartesiano x y) = atan (y/x)

--(e)



isLower :: Char -> Bool
isLower x = x >= 'a' && x <= 'z'

isDigit :: Char -> Bool
isDigit x = x >= '0' && x <= '9'

isAlpha :: Char -> Bool
isAlpha x = x >= 'a' && x <= 'z' || x >= 'A' && x <= 'Z'

toUpper :: Char -> Char
toUpper x = undefined



