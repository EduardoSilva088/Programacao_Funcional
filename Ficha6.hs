module Ficha6 where


--1

data BTree a = Empty
             | Node a (BTree a) (BTree a)
            deriving Show

t :: BTree Int
t = Node 5 (Node 3 (Node 9 Empty Empty)
                   (Node 8 (Node 15 Empty Empty)
                           Empty
                   )
           )
           (Node 7 (Node 4 Empty Empty)
                    Empty
           )

--(a)
altura :: BTree a -> Int
altura Empty = 0 
altura (Node i e d) = 1 + max ae ad
                    where ae = altura e
                          ad = altura d

--(b)
contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node i e d) = 1 + contaNodos e + contaNodos d

--(c)
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node i Empty Empty) = 1
folhas (Node i e d) = folhas e + folhas d

--(d)
prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 (Node i e d) = Empty
prune n (Node i e d) = Node i (prune (n-1) e) (prune (n-1) d)

--(e)
path :: [Bool] -> BTree a -> [a]
path [] _ = []
path _ Empty = []
path (x:xs) (Node i e d) | x == False = i : path xs e
                         | otherwise = i : path xs d

--(f)
mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node i e d) = Node i (mirror d) (mirror e)

--(g)
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f a Empty = Empty
zipWithBT f Empty a = Empty
zipWithBT f (Node i1 e1 d1) (Node i2 e2 d2) = Node (f i1 i2) (zipWithBT f e1 e2) (zipWithBT f d1 d2)

--(h)
unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT = undefined


--2

--(a)
minimo :: Ord a => BTree a -> a
minimo (Node i Empty d) = i
minimo (Node i e d) = minimo e

--(b)
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node i Empty d) = Empty
semMinimo (Node i e d) = (Node i (semMinimo e) d)

--(c)
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node i Empty d) = (i,Empty)
minSmin (Node i e d) = (minimo e, (Node i (semMinimo e) d))
-- minSmin (Node i e d) = (a, Node i b d)
--                       where (a,b) = minSmin e

--(d)
{-
remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x (Node i e d) | x < i = Node i (remove x e) d
                      | x > i = Node i e (remove x d)
                      | otherwise = aux x (Node e l r)
                      where aux n (Node i l r) = case l of 
                      	                       Empty -> r
                                               otherwise -> case r of 
                                               	            Empty -> l
                                                            otherwise -> Node g l h
                            (g,h) = minSmin r
-}

--3

type Aluno = (Numero,Nome,Regime,Classificacao)

type Numero = Int

type Nome = String

data Regime = ORD 
            | TE 
            | MEL 
            deriving Show

data Classificacao = Aprov Int
                   | Rep
                   | Faltou
                   deriving Show

type Turma = BTree Aluno --  árvore binária de procura (ordenada por número)

a1 :: Aluno
a1 = (10000,"Ana",ORD,Aprov 16)
a2 = (10010,"Rui",TE,Aprov 16)
a3 = (10020,"To",ORD,Aprov 16)
a4 = (10030,"Ze",TE,Aprov 16)
a5 = (10040, "Pedro",ORD,Aprov 16)

turma :: Turma
turma = Node a4 (Node a1 Empty
                         (Node a3 (Node a2 Empty Empty)
                          Empty))
                (Node a5 Empty Empty)

--(a)
inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum x (Node (nu,_,_,_) e d) | x == nu   = True
                                   | x < nu    = inscNum x e
                                   | otherwise = inscNum x d

--(b)
inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome x (Node (_,no,_,_) e d) | x == no = True
                                 | otherwise = inscNome x e || inscNome x d

--(c)
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (nu,no,TE,_) e d) = [(nu,no)] ++ trabEst e ++ trabEst d
trabEst (Node (nu,no,_,_) e d) = trabEst e ++ trabEst d

--(d)
nota :: Numero -> Turma -> Maybe Classificacao
nota n (Node (nu,no,_,cl) e d) | n /= nu = Nothing
                               | n == nu = Just cl
                               | n < nu = nota n e
                               | otherwise = nota n d

--(e)
percFaltas :: Turma -> Float
percFaltas Empty = 0
percFaltas t = sumFaltas t / sumAlunos t
                  where sumFaltas Empty = 0
                        sumFaltas (Node (_,_,_,Faltou) e d) = 1 + sumFaltas e + sumFaltas d
                        sumFaltas (Node (_,_,_,_) e d) = sumFaltas e + sumFaltas d
                        sumAlunos Empty = 0                                  
                        sumAlunos (Node (nu,_,_,_) e d) = 1 + sumAlunos e + sumAlunos d                                       

--(f)
mediaAprov :: Turma -> Float
mediaAprov Empty = 0 
mediaAprov t = sumNotas t / sumPassaram t
             where sumNotas Empty = 0
                   sumNotas (Node (_,_,_,Aprov x) e d) = fromIntegral x + sumNotas e + sumNotas d
                   sumNotas (Node (_,_,_,_) e d) = sumNotas e + sumNotas d
                   sumPassaram Empty = 0
                   sumPassaram (Node (_,_,_,Aprov x) e d) = 1 + sumPassaram e + sumPassaram d
                   sumPassaram (Node (_,_,_,_) e d) = sumPassaram e + sumPassaram d


--(g)                   
-- aprovAv :: Turma -> Float
-- aprovAv Empty = 0
-- aprovAv t =  a / b
--           where (a,b) = aux t
--                  aux Empty = (0,0)
--                  aux (Node (_,_,_,Aprov x) = (a+1,b) 
--                  aux (Node (_,_,_,Rep) = (a,b+1)
--                  aux (Node (_,_,_,Faltou) = (a,b)
                 

















