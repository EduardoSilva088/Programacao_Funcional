module Ficha7 where

data ExpInt = Const Int
              | Simetrico ExpInt
              | Mais ExpInt ExpInt
              | Menos ExpInt ExpInt
              | Mult ExpInt ExpInt
              deriving Show

 -- e = 3 + 4 * 5

-- infixa
e :: ExpInt
e = (Const 3) `Mais` ((Const 4) `Mult` (Const 5))

--prefixa

e' :: ExpInt
e' = Mais (Const 3) (Mult (Const 4) (Const 5))

--1

--(a)
calcula :: ExpInt -> Int
calcula (Const c)     = c
calcula (Simetrico e) = - (calcula e)
calcula (Mais e d)    = calcula e + calcula d
calcula (Menos e d)   = calcula e - calcula d
calcula (Mult e d)    = calcula e * calcula d

--(b)
infixa :: ExpInt -> String
infixa (Const c)     = show c
infixa (Simetrico e) = "( -" ++ infixa e ++ ")"
infixa (Mais e d)    = "(" ++ infixa e ++ "+" ++ infixa d ++ ")"
infixa (Menos e d)   = "(" ++ infixa e ++ "-" ++ infixa d ++ ")"
infixa (Mult e d)    = "(" ++ infixa e ++ "*" ++ infixa d ++ ")"

--(c)
posfixa :: ExpInt -> String
posfixa (Const c)      = show c
posfixa (Simetrico e)  = "-" ++ posfixa e
posfixa (Mais e d)     = posfixa e ++ " " ++ posfixa d ++ " +"
posfixa (Menos e d)    = posfixa e ++ " " ++ posfixa d ++ " -"
posfixa (Mult e d)     = posfixa e ++ " " ++ posfixa d ++ " *"




data RTree a = R a [RTree a]
             deriving Show



rt :: RTree Int
rt = R 5 [ R 2 [ R 3 []
               ]
         , R 7 []
         , R 4 [ R 11 [ R 13 []
                      ]
               , R 8 []
               ]
         ]
--2

--(a) - Soma os elementos da  árvore.
soma :: Num a => RTree a -> a
soma (R v l) = v + sum (map soma l)

--(b) - Calcula a altura da  árvore.
altura :: RTree a -> Int
altura (R v []) = 1
altura (R v l) = 1 + maximum (map altura l)

--(c) - Remove de uma  árvore todos os ele- mentos a
--   partir de uma determinada profundidade.
prune :: Int -> RTree a -> RTree a
prune 1 (R v l) = R v []
prune n (R v l) = R v (map (prune (n-1) )l )

--(d) - Gera a árvore simétrica.
mirror :: RTree a -> RTree a
mirror (R v []) = R v []
mirror (R v l) = R v (map mirror (reverse l))

--(e) - Corresponde à travessia postorder da árvore.
postorder :: RTree a -> [a]
postorder (R v []) = [v]
postorder (R v l) = (foldr (++) [] (map postorder l)) ++ [v] 











