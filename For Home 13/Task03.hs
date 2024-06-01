data BTree a = NullT | Node a (BTree a) (BTree a)
 deriving (Show, Eq)

sumSubT :: Num a => BTree a -> a
sumSubT NullT = 0
sumSubT (Node val left right) = val + sumSubT left + sumSubT right

maxSumSubT :: (Ord a, Num a) => BTree a -> a
maxSumSubT NullT = 0
maxSumSubT tree@(Node _ left right) = 
  maximum [sumSubT tree, maxSumSubT left, maxSumSubT right]

t1 :: (Num a) => BTree a 
t1 = Node 3 (Node 0 NullT NullT) (Node 2 (Node 0 NullT NullT) NullT )

t2 :: (Num a) => BTree a 
t2 = Node (-3) (Node 0 NullT NullT) (Node 2 (Node 0 NullT NullT ) NullT )

main :: IO ()
main = do
 print $ maxSumSubT t1 == 5
 print $ maxSumSubT t2 == 2
