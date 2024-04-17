import Data.Char

isPrime :: Int -> Bool
isPrime n = [1, n] == filter (\ d -> mod n d == 0) [1 .. n]

containsDigit :: Int -> Int -> Bool
containsDigit x y = elem (digitToInt $ head (show y)) (map digitToInt (show x))

getPrimesLC :: Int -> Int -> [Int]
getPrimesLC x y = [x | x <- [min x y .. max x y], containsDigit x 7 && isPrime x]

getPrimesHOF :: Int -> Int -> [Int]
getPrimesHOF x y = filter (\c -> isPrime c && containsDigit c 7) [min x y..max x y]

main :: IO()
main = do

 print $ getPrimesLC 1 100 == [7,17,37,47,67,71,73,79,97]
 print $ getPrimesLC 100 1 == [7,17,37,47,67,71,73,79,97]
 print $ getPrimesLC 3 30 == [7, 17] -- my test

 print $ getPrimesHOF 1 100 == [7,17,37,47,67,71,73,79,97]
 print $ getPrimesHOF 100 1 == [7,17,37,47,67,71,73,79,97]
 print $ getPrimesHOF 4 40 == [7, 17, 37] -- my test