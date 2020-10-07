module AulaArvores where
import Data.Maybe


data BTree a = Empty | Node a (BTree a) (BTree a)
data Maybe a = Nothing | Just a

lookupABP :: Eq a => a -> BTree(a,b) -> Maybe b
lookupABP x Empty = Nothing
lookupABP x (Node (y,z) e d) | x == y = (Just z)
                             | x < y = lookupABP x e 
                             | x > y = lookupABP x d 

mensagem :: Int -> BTree (Int,String) -> String
mensagem n turma = case (lookupABP n turma) of
	               Nothing -> "Não existe"
	               (Just x) -> "Nome:" ++ z

remove :: Ord a => a -> BTree a -> BTree a
remove x Empty = Empty
remove x (Node y e d) | x < y = Node y (remove x e) d
                      | x > y = Node y e (remove x d)
                      | x == y = let ( m,e') 
