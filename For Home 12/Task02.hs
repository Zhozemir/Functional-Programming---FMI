data BTree = Nil | Node Int BTree BTree
 deriving (Show, Eq)

tree :: BTree
tree = Node 4 (Node 1 (Node 0 Nil Nil) (Node 2 Nil (Node 3 Nil Nil))) (Node 6 (Node 5 Nil Nil) (Node 7 Nil (Node 8 Nil Nil)))

tree2 :: BTree
tree2 = Node 10 (Node 5 (Node 2 Nil Nil) (Node 7 Nil (Node 8 Nil Nil))) (Node 15 Nil (Node 20 (Node 17 Nil Nil) Nil)) -- my test

convert :: BTree -> BTree
convert tree = fst (convertHelper tree 0)
  where
    convertHelper :: BTree -> Int -> (BTree, Int)
    convertHelper Nil acc = (Nil, acc)
    convertHelper (Node val left right) acc =
        (Node newVal leftConverted rightConverted, leftSum)
      where
        (rightConverted, rightSum) = convertHelper right acc
        newVal = val + rightSum
        (leftConverted, leftSum) = convertHelper left newVal

main :: IO ()
main = do
 print $ convert tree == (Node 30 (Node 36 (Node 36 Nil Nil) (Node 35 Nil (Node 33 Nil Nil))) (Node 21 (Node 26 Nil Nil) (Node 15 Nil (Node 8 Nil Nil))))
 print $ convert tree2 == Node 62 (Node 82 (Node 84 Nil Nil) (Node 77 Nil (Node 70 Nil Nil))) (Node 52 Nil (Node 20 (Node 37 Nil Nil) Nil)) -- my test
    
