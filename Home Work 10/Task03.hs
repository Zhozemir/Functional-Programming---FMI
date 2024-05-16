import Data.List 

data Tree a = Nil | Node (a, a) (Tree a) (Tree a) 
 deriving (Show)

traverseDFS :: (Ord a) => Tree a -> [(a, a)]
traverseDFS Nil = []
traverseDFS (Node value left right) = traverseDFS left ++ [value] ++ traverseDFS right

ordered :: (Ord a) => Tree a -> Bool
ordered Nil = True
ordered tree = sort (traverseDFS tree) == reverse (traverseDFS tree)

--ordered :: (Ord a) => Tree a -> Bool
--ordered Nil = True
--ordered tree = sortedValues == reverse sortedValues
  --where sortedValues = sort (traverseDFS tree)

t1 :: Tree Int
t1 = Node (3, 10) (Node (5, 8) (Node (6, 7) Nil Nil) (Node (4, 9) Nil Nil)) (Node (2, 12) Nil (Node (1, 15) Nil Nil))

t2 :: Tree Int
t2 = Node (3, 10) (Node (5, 8) (Node (6, 7) Nil Nil) (Node (7, 9) Nil Nil)) (Node (2, 12) Nil (Node (1, 15) Nil Nil))

t3 :: Tree Int
t3 = Node (3, 5) (Node (7, 1) (Node (4, 3) Nil Nil) (Node (6, 9) Nil Nil)) (Node (3, 6) Nil (Node (1, 4) Nil Nil)) 

main :: IO ()
main = do 
 print $ ordered t1 == True
 print $ ordered t2 == False
 print $ ordered t3 == False -- my test
  
