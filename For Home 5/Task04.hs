import Data.List

specialSum :: Int -> Int -> Int
specialSum x y = sum [n | n <- [x..y], elem '6' (show n), mod (n - 1) 4 == 0]

main :: IO()
main = do

 print $ specialSum 1 100 == 195
 print $ specialSum 100 200 == 495 -- my test 
