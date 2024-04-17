import Data.List

rf :: (Int -> Int) -> (Int -> Int) -> [Int] -> (Int -> Int) -> [Int]
rf f g xs h = [h x | x <- xs, f x > g x]

main :: IO()
main = do 
 print $ (rf ((-) (-4)) (* (-2))) [1..10] (* 3) == [15,18,21,24,27,30]
 print $ (rf (+ 2) (+ 1)) [1..5] (* 2) == [2, 4, 6, 8, 10] -- my test