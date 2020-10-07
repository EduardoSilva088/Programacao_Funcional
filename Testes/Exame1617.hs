module Exame1617 where 

--1
--(a)
unlines' :: [String] -> String
unlines' [] = " "
unlines' (x:xs) = "x" ++ "\n" ++ unlines' xs

--(b)
(\\) :: (Eq a) => [a] -> [a] -> [a]
(\\) [] _ = []
(\\) l [] = l
(\\) l (y:ys) = (\\) (delete y l) ys
              where delete n [] = []
                    delete n (x:xs) | n == x = xs
                                    | otherwise = x : delete n xs


--2

data Seq a = Nil 
           | Inicio a (Seq a) 
           | Fim (Seq a) a

--(a)

primeiro :: Seq a -> a
primeiro (Inicio a s) = a
primeiro (Fim Nil a) = a
primeiro (Fim s a) = primeiro s

--(b)
semUltimo :: Seq a -> Seq a
semUltimo (Inicio a Nil) = Nil
semUltimo (Inicio a s) = semUltimo s
semUltimo (Fim s a) = s

--3

data BTree a = Empty 
             | Node a (BTree a) (BTree a)

--(a)
prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 (Node i e d) = Empty
prune n (Node i e d) = (Node i (prune (n-1) e) (prune (n-1) d))

--(b)             
semMinimo :: (Ord a) => BTree a -> BTree a
semMinimo Empty = Empty
semMinimo (Node i Empty d) = d
semMinimo (Node i e d) = Node i (semMinimo e) d

--4
type Tabuleiro = [String]

exemplo :: Tabuleiro
exemplo = [". . R .",
           "R . . .",
           ". . . R",
           ". R . ."]

--(a)
posicoes :: Tabuleiro -> [(Int,Int)]
posicoes t = undefined           















