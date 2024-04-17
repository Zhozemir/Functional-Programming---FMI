import Data.List

getOddCompositionValue :: [Int -> Int] -> Int -> Int
getOddCompositionValue fs x = foldr1 (.) (map fst $ filter (odd . snd) $ zip fs [1..]) $ x

main :: IO()
main = do

 print $ (getOddCompositionValue [(\x -> x + 1),(\x -> x * 2),(\x -> x - 1), (\x -> div x 2)]) 2 == 2
 print $ (getOddCompositionValue [(\x -> x + 2), (\x -> x * 3), (\x -> x + 3), (\x -> mod x 2)]) 4 == 9 -- my test