module Questoes where
import Data.Char
import Data.List
import Data.Either

--(1)
myEnumFromTo :: Int -> Int -> [Int]
myEnumFromTo x y | x >= y = [y]
                 | x < y = x : myEnumFromTo (x+1) y

--(2)
myenumFromThenTo :: Int -> Int -> Int -> [Int]
myenumFromThenTo x y z | x < y && x < z  = x : myenumFromThenTo y (y+(y-x)) z
                       | x == y = repeat x
                       | x == z = [x]
                       | otherwise = []


--(3)
(+++) :: [a] -> [a] -> [a]
(+++) [] [] = []
(+++) [] y = y
(+++) (x:xs) y = x : (xs+++y)

--(4)
(!!!) :: [a] -> Int -> a
(!!!) (x:xs)  0 = x
(!!!) (x:xs) n = (!!!) xs (n-1)

--(5)
myreverse :: [a] -> [a] 
myreverse [] = []
myreveree (x:xs) = (myreverse xs) +++ [x]

--(6)
mytake :: Int -> [a] -> [a]
mytake 0 _ = []
mytake _ [] = []
mytake n (x:xs)
       | n>0 = x : mytake (n-1) xs
       | otherwise = []


--7
mydrop :: Int -> [a] -> [a]
mydrop 0 l = l
mydrop _ [] = []
mydrop n (x:xs)
      | n>0 = mydrop (n-1) xs
      | otherwise = x:xs 

--8
myzip :: [a] -> [b] -> [(a,b)]
myzip _ [] = []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys

--unzipp
myunzip :: [(a,b)] -> ([a],[b])
myunzip [] = ([],[])
myunzip ((x,y):t) = (x:fst(myunzip t), y:snd(myunzip t))

--unzipp2
myunzip2 :: [(a,b)] -> ([a],[b])
myunzip2 [] = ([],[])
myunzip2 ((x,y):t) = (x:l1 , y:l2)
          where (l1,l2) = myunzip2 t

--9
myelem :: Eq a => a -> [a] -> Bool
myelem _ [] = False
myelem a (x:xs)
             | x==a = True
             | x/=a && myelem a xs = True
             | otherwise = False 

--10
myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate x y = y : myreplicate (x-1) y

--11
myintersperce :: a -> [a] -> [a]
myintersperce n [x] = [x]
myintersperce y (x:xs) = x : y : myintersperce y xs

--12
mygroup :: Eq a => [a] -> [[a]]
mygroup [] = []
mygroup [x] = [[x]]
mygroup (x:y:xs) | x == y = [x] : [y] : mygroup xs
                 | x /= y = [x] : [y] : mygroup xs

mgroup :: Eq a => [a] -> [[a]]
mgroup [] = [[]]
mgroup (x:xs) = aux6 [x] xs
              where aux6 a [] = [a]
                    aux6 a (x:xs) | elem x a = aux6 (x:a) xs
                                  | otherwise = a : aux6 [x] xs

--13
myconcat :: [[a]] -> [a] 
myconcat [] = []
myconcat (x:xs) = x +++ myconcat xs

--14
myinits :: [a] -> [[a]]
myinits [] = []
myinits l = myinits (init l) +++ [l]

--15
mytails :: [a] -> [[a]]
mytails [] = []
mytails l = [l] +++ mytails (tail l)

--16
myisPrefixOf :: Eq a => [a] -> [a] -> Bool
myisPrefixOf [] [] = True
myisPrefixOf [] _ = True
myisPrefixOf _ [] = False
myisPrefixOf (x:xs) (y:ys) = if x == y
                           then myisPrefixOf xs ys
                           else False
--17
myisSuffixOf :: Eq a => [a] -> [a] -> Bool
myisSuffixOf [] _ = True
myisSuffixOf l1 l2 = if (last l1) == (last l2)
                           then myisSuffixOf (init l1) (init l2)
                           else False

--18
myisSubsequenceOf :: Eq a => [a] -> [a] -> Bool
myisSubsequenceOf [] _ = True
myisSubsequenceOf _ [] = False
myisSubsequenceOf (x:xs) (y:ys) = if x == y 
                                then myisSubsequenceOf xs ys
                                else myisSubsequenceOf (x:xs) ys

--19 aux
myelemIndices :: Eq a => a -> [a] -> [Int]
myelemIndices _ [] = []
myelemIndices x l  = aux 0 x l
                  where
                    aux _ _ [] = []
                    aux i x (h:t) = if x==h
                                    then i:aux (i+1) x t
                                    else aux (i+1) x t


--20
mynub :: Eq a => [a] -> [a]
mynub [] = []
mynub (x:xs) = x : mynub (aux4 x xs)
               where aux4 _ [] = []
                     aux4 n (x:xs) = if x == n 
                                     then aux4 n xs 
                                     else x : aux4 n xs
--21
mydelete :: Eq a => a -> [a] -> [a]
mydelete _ [] = []
mydelete x (y:ys) = if x == y
                  then ys
                  else x : mydelete x ys

--22

(\\\) :: Eq a => [a] -> [a] -> [a]
(\\\) l [] = l
(\\\) [] _ = []
(\\\) l (y:ys) = (\\\) (delete y l) ys

--23 
myunion :: Eq a => [a] -> [a] -> [a]
myunion [] [] = []
myunion l [] = l
myunion [] l = l
myunion l (x:xs) | myelem x l = myunion l xs
                 | otherwise = myunion (l ++ [x]) xs


--24
myintersect :: Eq a => [a]-> [a] -> [a]
myintersect [] [] = []
myintersect [] l = []
myintersect l [] = []
myintersect (x:xs) l | myelem x l = x : myintersect l xs
                     | otherwise = myintersect l xs


--25
myinsert :: Ord a => a -> [a] -> [a]
myinsert x [] = [x]
myinsert x (y:ys)
               |x <= y = x:y:ys
               |x > y = y: myinsert x ys

--26
unwords :: [String] -> String
unwords [] = " "
myunwords [x] = x
myunwords (x:xs) = x +++ " " +++ myunwords xs

--27
myunlines :: [String] -> String
myunlines [] = ""
myunlines [x] = x +++ "\n"
myunlines (x:xs) = x +++ "\n" +++ myunlines xs

--28 AUX
mypMaior :: Ord a => [a] -> Int
mypMaior [x] = 0
mypMaior (h:t) | h == aux3 (h:t) = 0
               | otherwise = 1 + mypMaior t
                where aux3 [x] = x
                      aux3 (x:y:xs) | x>= y = aux3 (x:xs)
                                    | otherwise = aux3 (y:xs)

--29
mytemRepetidos :: Eq a => [a] -> Bool
mytemRepetidos [] = False
mytemRepetidos (x:xs) | myelem x xs = True
                      | mytemRepetidos xs = True 
                      | otherwise = False 

--30
myalgarismos :: [Char] -> [Char]
myalgarismos [] = []
myalgarismos (x:xs) = if isDigit x then x : myalgarismos xs else myalgarismos xs

--31
myposImpares :: [a] -> [a]
myposImpares [] = []
myposImpares [x] = []
myposImpares (x:y:xs) = y : myposImpares xs

--32
myposPares :: [a] -> [a]
myposPares [] = []
myposPares [x] = [x]
myposPares (x:y:xs) = x : myposPares xs

--33
myisSorted :: Ord a => [a] -> Bool
myisSorted [] = True
myisSorted [x] = True
myisSorted (x:y:xs) = if y > x then myisSorted (y:xs) else False

--34
myiSort :: Ord a => [a] -> [a]
myiSort [] = []
myiSort [x] = [x]
myiSort (x:xs) = myinsert x (myiSort xs) 


--35
mymenor :: String -> String -> Bool
mymenor [] _ = True
mymenor _ [] = False
mymenor (x:xs) (y:ys) | ord x < ord y = True
                      | ord x > ord y = False
                      | otherwise = mymenor xs ys

--36
myelemMSet :: Eq a => a -> [(a,Int)] -> Bool
myelemMSet a [] = False
myelemMSet a ((x,n):xs) = if a == x then True else myelemMSet a xs


--37
mylenghtMSet :: [(a,Int)] -> Int
mylenghtMSet [] = 0
mylenghtMSet ((x,n):xs) = n + mylenghtMSet xs


--38
myconverteMSet :: [(a,Int)] -> [a]
myconverteMSet [] = []
myconverteMSet ((x,n):xs) = aux2 (x,n) +++ myconverteMSet xs
                          where aux2 (x,0) = []
                                aux2 (x,n) = [x] +++ aux2 (x,n-1)

--39
myinsereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
myinsereMSet n [] = [(n,1)]
myinsereMSet n ((x,a):xs) | n == x = (x,a+1):xs
                          | otherwise = (x,a) : myinsereMSet n xs
 
--40
myremoveMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
myremoveMSet n [] = []
myremoveMSet n ((x,a):xs) | n == x && a == 1 = xs
                          | n == x && a > 1 = ((x,a-1):xs)
                          | otherwise = (x,a) : myremoveMSet n xs

--41
myconstroiMSet :: Ord a => [a] -> [(a,Int)] 
myconstroiMSet l = aux8 1 l
               where
                 aux8 i [x] = [(x,i)]
                 aux8 i (x:y:xs) | x==y = aux8 (i+1) (x:xs)
                                | x/=y = (x,i):aux8 1 (y:xs)



--42
mypartitionEithers :: [Either a b] -> ([a],[b])
mypartitionEithers l = (left l, right l)
                    where left (Left x:xs) = x : left xs
                          left (Right x:xs) = left xs
                          left _ = []
                          right (Left x:xs) = right xs
                          right (Right x:xs) = x : right xs
                          right _ = []

--43
mycatMaybes :: [Maybe a] -> [a]
mycatMaybes [] = []
mycatMaybes (Just x : xs) = x : mycatMaybes xs
mycatMaybes (Nothing : xs) = mycatMaybes xs


--MAPAS

data Movimento = Norte | Sul | Este | Oeste
                  deriving Show
--44 

myposicao :: (Int,Int) -> [Movimento] -> (Int,Int)
myposicao (x,y) [] = (x,y)
myposicao (x,y) (Norte:t) = myposicao (x,y+1) t
myposicao (x,y) (Sul:t) = myposicao (x,y-1) t
myposicao (x,y) (Este:t) = myposicao (x-1,y) t
myposicao (x,y) (Oeste:t) = myposicao (x+1,y) t


--45
mycaminho :: (Int,Int) -> (Int,Int) -> [Movimento]
mycaminho (x1,y1) (x2,y2) | x1 == x2 && y1 == y2 = []
                          | x1 < x2 = [Este] +++ mycaminho (x1+1,y1) (x2,y2)
                          | x1 > x2 = [Oeste] +++ mycaminho (x1-1,y1) (x2,y2)
                          | y1 < y2 = [Norte] +++ mycaminho (x1,y1+1) (x2,y2)
                          | y1 > y2 = [Sul] +++ mycaminho (x1,y1-1) (x2,y2)

--46
myvertical :: [Movimento] -> Bool
myvertical [] = True
myvertical (Este:xs) = False
myvertical (Oeste:xs) = False
myvertical (Norte:xs) = myvertical xs
myvertical (Sul:xs) = myvertical xs

--POSICAO

data Posicao = Pos Int Int
                  deriving Show

--47
mymaisCentral :: [Posicao] -> Posicao
mymaisCentral = undefined


--48
myvizinhos :: Posicao -> [Posicao] -> [Posicao]
myvizinhos _ [] = []
myvizinhos (Pos x y) ((Pos k w):t) = if (x == k && y == w-1 || x == k && y == w+1 || y == w && x == k+1 || y == w && x == k-1)
                                     then (Pos k w) : myvizinhos (Pos x y) t
                                     else myvizinhos (Pos x y) t

--49
mymesmaOrdenada :: [Posicao] -> Bool 
mymesmaOrdenada [x] = True
mymesmaOrdenada ((Pos x y):(Pos k w):t) | y == w = True
                                        | y /= w = mymesmaOrdenada ((Pos k w):t)
                                        | otherwise = False

--SEMAFORO

data Semaforo = Verde | Amarelo | Vermelho
                   deriving Show

--50
interseccaoOK :: [Semaforo] -> Bool
interseccaoOK l = aux7 l <= 1
           where aux7 [Vermelho] = 0
                 aux7 [Verde] = 1
                 aux7 [Amarelo] = 1
                 aux7 (Vermelho:resto) = aux7 resto
                 aux7 (Verde:resto) = 1 + aux7 resto
                 aux7 (Amarelo:resto) = 1 + aux7 resto
