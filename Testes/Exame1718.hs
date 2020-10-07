module Exame1718 where 

--1 
(!!!) :: [a] -> Int -> a
(!!!) (x:xs) n | n == 0 = x 
               | n > 0 = (!!!) xs (n-1)

--2
data Movimento = Norte 
               | Sul 
               | Este 
               | Oeste 
               deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (Norte:xs) = posicao (x,y+1) xs
posicao (x,y) (Sul:xs) = posicao (x,y-1) xs
posicao (x,y) (Este:xs) = posicao (x+1,y) xs
posicao (x,y) (Oeste:xs) = posicao (x-1,y) xs


--3
any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (x:xs) = f x || any' f xs

--4

type Mat a = [[a]]


triSup :: Num a => Mat a -> Bool
triSup [] = False
triSup 

filter (/= 0) length reverse l

--5
movimenta :: IO (Int,Int)
movimenta = moveDe (0,0)

moveDe :: (Int,Int) -> IO (Int,Int)
moveDe (x,y) = do 
             dir <- getChar
             case dir of 
                   'n' -> moveDe (x,y+1)
                   's' -> moveDe (x,y-1)
                   'e' -> moveDe (x+1,y)
                   'o' -> moveDe (x-1,y)
                   otherwise -> return (x,y)


--6

data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]                   

ex :: Imagem
ex = Mover (5,5)
      (Juntar [Mover (0,1) (Quadrado 5),
              Quadrado 4,
              Mover (4,3) (Quadrado 2)])

ex2 :: Imagem
ex2 = Juntar []

--(a)
vazia :: Imagem -> Bool
vazia (Quadrado _) = False
vazia (Mover (_,_) i) = vazia i
vazia (Juntar li) | null li = True
                  | otherwise = or (map vazia li) 

--(b)
maior :: Imagem -> Maybe Int
maior (Quadrado x) = Just x
maior (Mover _ i) = maior i
maior (Juntar li) | null li = Nothing
                  | otherwise = maximum $ map maior li