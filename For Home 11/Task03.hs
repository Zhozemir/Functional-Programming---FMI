import Data.List

data Color = Red | Green | Blue 
 deriving (Show, Eq)
data Tree = Empty | Node Color Tree Tree 
 deriving (Show, Eq)

colorTree :: Tree
colorTree = Node Blue (Node Red (Node Green Empty Empty) (Node Red (Node Green Empty Empty) (Node Red Empty Empty))) (Node Red (Node Blue (Node Green Empty Empty) (Node Red Empty Empty)) Empty)

colorTree2 :: Tree
colorTree2 = Node Green (Node Red (Node Blue Empty Empty) (Node Blue Empty Empty)) (Node Blue (Node Green Empty Empty) (Node Blue (Node Red Empty Empty) (Node Blue Empty Empty)))

maxDepthBlueNode :: Tree -> Int
maxDepthBlueNode tree = maxDepthHelper tree 0
  where
    maxDepthHelper :: Tree -> Int -> Int
    maxDepthHelper Empty _ = 0
    maxDepthHelper (Node Blue left right) depth = 
      max depth $ max (maxDepthHelper left $ depth + 1) (maxDepthHelper right $ depth + 1)
    maxDepthHelper (Node _ left right) depth =
      max (maxDepthHelper left $ depth + 1) (maxDepthHelper right $ depth + 1)

main :: IO ()
main = do
  print $ maxDepthBlueNode colorTree == 2 
  print $ maxDepthBlueNode colorTree2 == 3 -- my test
