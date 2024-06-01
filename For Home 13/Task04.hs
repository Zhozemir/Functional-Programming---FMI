data NTree a = NullT | Node a [NTree a] 
 deriving (Show, Eq)

isBoring :: (Eq a) => NTree a -> Bool
isBoring NullT = True
isBoring (Node x children) = all (== x) childValues && all isBoring children
  where
    childValues = [v | Node v _ <- children]

t1 :: NTree Int
t1 = Node 10 [Node 10 [Node 10 [NullT], Node 8 [Node 10 [NullT]], Node 2 [NullT]], Node 10 [Node 11 [NullT], Node 10 [NullT], Node 6 [NullT]]]

t2 :: NTree Char
t2 = Node 's' [Node 's' [NullT], Node 's' [NullT], Node 's' [NullT]]

main = do
 print $ isBoring t1 == False
 print $ isBoring t2 == True
