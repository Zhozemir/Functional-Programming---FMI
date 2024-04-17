import Data.List

sumUnique :: [[Int]] -> Int
sumUnique = sum . map head . filter (\x -> length x == 1) . concatMap (filter (\y -> length y == 1) . group . sort)

main :: IO ()
main = do

 print $ sumUnique [[1,2,3,2],[1,-4],[1]] == 2
 print $ sumUnique [[1,2,3,2],[-4,-4],[5]] == 9 
 print $ sumUnique [[2,2,2],[3,3,3],[4,4,4]] == 0
 print $ sumUnique [[1,2,3],[4,5,6],[7,8,9]] == 45
 print $ sumUnique [[1,1,4,5,5],[4,4,6,1,1],[7,7,8,9,9]] == 18 -- my test
