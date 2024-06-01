import Data.List

data BTree = Nil | Node Int BTree BTree

t1 :: BTree
t1 = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 26 Nil Nil) (Node 32 Nil Nil)))

t2 :: BTree
t2 = Node 25 (Node 10 (Node 1 Nil Nil) Nil) (Node 30 (Node 32 Nil Nil) (Node 26 Nil Nil))

t3 :: BTree
t3 = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 26 Nil Nil) (Node 32 Nil Nil)))

t4 :: BTree
t4 = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 27 Nil Nil) (Node 32 Nil Nil)))

t5 :: BTree
t5 = Node 30 (Node 3 Nil Nil) (Node 23 Nil (Node 13 (Node 21 Nil Nil) (Node 34 Nil Nil))) 

t6 :: BTree
t6 = Node 21 (Node 11 Nil Nil) (Node 5 Nil (Node 6 (Node 7 Nil Nil) (Node 84 Nil Nil)))

getLeaves :: BTree -> [Int]
getLeaves Nil = []
getLeaves (Node x Nil Nil) = [x]
getLeaves (Node _ left right) = getLeaves left ++ getLeaves right

leavesAreEqual :: BTree -> BTree -> Bool
leavesAreEqual bt1 bt2 = sort (getLeaves bt1) == sort (getLeaves bt2)

main :: IO ()
main = do
 print $ leavesAreEqual t1 t2 == True
 print $ leavesAreEqual t3 t4 == False
 print $ leavesAreEqual t5 t6 == False -- my test 
    
