import Data.List

onlyArithmetic :: [[Int]] -> [[Int]]
onlyArithmetic = filter isArithmetic

isArithmetic :: [Int] -> Bool
isArithmetic [] = False
isArithmetic [_] = True
isArithmetic [x, y] = True
isArithmetic (x:y:z:xs) = (y - x) == (z - y) && isArithmetic (y:z:xs)

main :: IO()
main = do
 print $ onlyArithmetic [[3], [1, 2, 3, 4, 5], [3, 5, 8, 9, 11]]  == [[3], [1, 2, 3, 4, 5]]
 print $ onlyArithmetic [[1, 3, 5], [3, 4, 6], [4, 7, 10]] == [[1, 3, 5], [4, 7, 10]] -- my test