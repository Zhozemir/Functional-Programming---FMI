import Data.List
import Data.Char

type Node = Int
type Graph = [(Node, [Node])]
type Path = [Node]

findNeighbors :: Node -> Graph -> [Node]
findNeighbors n [] = error "Starting node not found in graph" 
findNeighbors n ((node, neighbors):rest)
    | n == node = neighbors
    | otherwise = findNeighbors n rest

simplePaths :: Graph -> Int -> Node -> [Path]
simplePaths graph k n
  | k < 0 = error "This is negative length"
  | k == 0 = [[n]]
  | otherwise = concatMap (\neighbor -> map (\path -> n : path) (simplePaths graph (k - 1) neighbor)) (findNeighbors n graph)

main :: IO ()
main = do
  print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 0 1 == [[1]]
  print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 1 == [[1, 2], [1, 3]]
  print $ simplePaths [(1, [2, 3, 4]), (2, [3, 4]), (3, []), (4, [])] 1 1 == [[1, 2], [1, 3], [1, 4]]
  print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 2 1 == [[1, 2, 3], [1, 2, 4]]
  print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 2 == [[2, 3], [2, 4]]
  print $ simplePaths [(1, [2, 3]), (2, [3]), (3, []), (4, [])] 1 2 == [[2, 3]]
  print $ simplePaths [(1, [2]), (2, [3]), (3, [4]), (4, [])] 3 1 == [[1, 2, 3, 4]] -- my test
