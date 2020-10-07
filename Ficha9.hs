module Ficha9 where

import System.Random

--1

--(a)

bingo :: IO ()
bingo = do lb <- acumNums []
           print lb
           return ()

acumNums :: [Int] -> IO [Int]
acumNums l | length l == 90 = return l
           | otherwise = do c <- getChar
                            n <- randomRIO (1,90)
                            print n
                            let nl = if elem n l then l 
                                     else (n:l) in acumNums nl

--(b)

geraChaveSecreta :: IO (Char,Char,Char,Char)
geraChaveSecreta = do c1 <- randomRIO ('0','9')
                      c2 <- randomRIO ('0','9')
                      c3 <- randomRIO ('0','9')
                      c4 <- randomRIO ('0','9')
                      return (c1,c2,c3,c4)

introSeq :: IO (Char,Char,Char,Char)
introSeq = do a <- getChar
              b <- getChar
              c <- getChar
              d <- getChar
              return (a,b,c,d)

introSeq' :: IO (Char,Char,Char,Char)
introSeq' = do str <- getLine
               return (str !! 0, str !! 1, str !! 2, str !! 3)

iguaisAChave :: (Char,Char,Char,Char) -> (Char,Char,Char,Char) -> Int
iguaisAChave (c1,c2,c3,c4) (p1,p2,p3,p4) = length $ filter (== True) [c1 == p1, c2 == p2, c3 == p3, c4 == p4] 

type Chave = (Char,Char,Char,Char)

jogar :: Chave -> Chave -> IO ()
jogar c p | iguaisAChave c p == 4 = do putStr "Parabéns! Descobriste :D "
          | otherwise = do putStr ("Tem" ++ " " ++ (show (iguaisAChave c p )) ++ " " ++ "lugares certos.")
                           putStr "Introduza nova jogada."
                           np <- introSeq' 
                           jogar c np

masterMind :: IO ()
masterMind = do cs <- geraChaveSecreta
                putStr "Introduza uma jogada:\n"
                p <- introSeq'
                jogar cs p 



