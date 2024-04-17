import Data.List 

type Node = Int
type TreeNode = (Node, Node, Node)
type BinaryTree = [TreeNode]

listLeaves :: BinaryTree -> [Node]
listLeaves tree = nub [child | child <- children, all (/= child) parents]
  where
    parents = [x | (x, _, _) <- tree]    
    children = [y | (_, y, _) <- tree]   
             ++ [z | (_, _, z) <- tree]  

main :: IO ()
main = do
  print $ listLeaves [(1, 2, 3), (2, 4, 5)] == [4, 3, 5]
  print $ listLeaves [(2, 4, 5), (1, 2, 3)] == [4, 5, 3]
  print $ listLeaves [(1, 2, 3), (3, 4, 5), (5, 6, 9)] == [2, 4, 6, 9]
  print $ listLeaves [(1, 2, 3), (2, 3, 6), (3, 4, 5)] == [4, 6, 5] -- my test
