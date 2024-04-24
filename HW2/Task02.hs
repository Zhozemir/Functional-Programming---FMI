import Data.List

charToDirection :: Char -> Int
charToDirection 'R' = 1
charToDirection 'L' = -1

setupRobots :: [Int] -> String -> (Int -> [Int])
setupRobots xs ms = 
    \t -> (sort . map ((\(pos, dir) -> pos + dir * t) . (\(pos, dir) -> (pos, dir))) . zip xs . map charToDirection) ms

main :: IO ()
main = do
    print $ setupRobots [0, 1] "LR" 3 == [-3, 4]
    print $ setupRobots [-2, 0, 2] "RLL" 2 == [-2, 0, 0]
    print $ setupRobots [-2, 0, 2] "RLL" 5 == [-5, -3, 3]
    print $ setupRobots [-2, 0, 1, 3, 4, 7, 10, 12, 15] "RLLLRRLRL" 1 == [-1, -1, 0, 2, 5, 8, 9, 13, 14]
    print $ setupRobots [-2, 0, 1, 3, 4, 7, 10, 12, 15] "RLLLRRLRL" 3 == [-3, -2, 0, 1, 7, 7, 10, 12, 15]
    print $ setupRobots [-2, 0, 1, 3, 4, 7, 10, 12, 15] "RLLLRRLRL" 5 == [-5, -4, -2, 3, 5, 9, 10, 12, 17]
