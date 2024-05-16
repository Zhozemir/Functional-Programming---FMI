data NTree a = Nil | Node a [NTree a] 
 deriving (Show, Eq)

isGraceful :: Integral a => NTree a -> Bool
isGraceful Nil = True    
isGraceful (Node parent children) =
    all (\child -> even $ abs (parent - value child) ) children && all isGraceful children
    where
        value Nil = parent 
        value (Node v _) = v

t1 :: NTree Int
t1 = Node 1 [Node 3 [Nil], Node 5 [Nil], Node 7 [Nil], Node 9 [Nil]]

t2 :: NTree Int
t2 = Node 7 [Node 9 [Node 5 [Nil], Node 2 [Nil]], Nil]

t3 :: NTree Int
t3 = Node 4 [Node 2 [Nil], Node 6 [Node 8 [Nil], Node 10 [Nil]]]

main :: IO ()
main = do
 print $ isGraceful t1 == True
 print $ isGraceful t2 == False
 print $ isGraceful t3 == True -- my test
