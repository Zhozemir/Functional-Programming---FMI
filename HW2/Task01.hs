warmerAfter :: [Double] -> [Int]
warmerAfter [] = []
warmerAfter ts = map (\i -> daysUntilWarmer i ts) [0..length ts - 1]
  where
    daysUntilWarmer :: Int -> [Double] -> Int
    daysUntilWarmer i ts
      | i == length ts - 1 = 0
      | otherwise = findWarmer i (drop (i + 1) ts) $ i + 1

    findWarmer :: Int -> [Double] -> Int -> Int
    findWarmer _ [] _ = 0
    findWarmer i (x:xs) day
      | x > (ts !! i) = day - i
      | otherwise = findWarmer i xs $ day + 1

main :: IO()
main = do 
  print $ warmerAfter [20,21,20,19,18,20,25,24,23,20,26] == [1,5,4,2,1,1,4,3,2,1,0]
  print $ warmerAfter [0,10,20,30] == [1,1,1,0]
  print $ warmerAfter [21,22,23] == [1,1,0]
  print $ warmerAfter [23,24,25,21,19,23,26,23] == [1,1,4,2,1,1,0,0]
