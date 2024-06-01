data BTree = Nil | Node Int BTree BTree 
 deriving (Show)

numberBTree :: BTree
numberBTree = Node 10 (Node 5 (Node 1 Nil Nil) (Node 9 Nil Nil)) (Node 6 (Node 8 Nil Nil) (Node 7 Nil Nil))

newNumberBTree :: BTree
newNumberBTree = Node 15 (Node 8 (Node 5 Nil Nil) (Node 10 Nil Nil)) (Node 20 (Node 17 Nil Nil) (Node 25 Nil Nil))

levelSum :: BTree -> Int -> Int
levelSum Nil _ = 0
levelSum (Node value _ _) 0 = value
levelSum (Node _ left right) k = levelSum left (k - 1) + levelSum right (k - 1)

cone :: BTree -> Bool
cone tree = checkCone tree 0 (-1)
    where
        checkCone :: BTree -> Int -> Int -> Bool
        checkCone Nil _ _ = True
        checkCone (Node _ left right) level prevSum
            | currSum == 0 = True
            | prevSum >= 0 && currSum <= prevSum = False
            | otherwise = checkCone tree (level + 1) currSum
            where 
                currSum = levelSum tree level


main :: IO ()
main = do
 print $ levelSum numberBTree 1 == 11 -- (5 + 6)
 print $ cone numberBTree == True
 print $ levelSum newNumberBTree 1 == 28
 print $ cone newNumberBTree == True
