import Data.List

data Measuring = Temp Int Float 
 deriving (Show)

closestAverage :: [Measuring] -> Int
closestAverage [] = error "Empty list"
closestAverage xs = day closest
  where
    avg = sum [temp | Temp _ temp <- xs] / fromIntegral (length xs)
    closest = foldr1 findClosest xs
    findClosest (Temp day1 temp1) (Temp day2 temp2)
      | abs (temp1 - avg) < abs (temp2 - avg) = Temp day1 temp1
      | otherwise = Temp day2 temp2
    day (Temp d _) = d

main :: IO()
main = do
  print $ closestAverage [(Temp 1 23.6), (Temp 6 24.2), (Temp 11 24.2), (Temp 16 21.2), (Temp 21 23.8), (Temp 26 26.5), (Temp 31 24.5)] == 21
  print $ closestAverage [(Temp 3 22.3), (Temp 8 24.1), (Temp 13 23.9), (Temp 18 26.7), (Temp 23 27.9), (Temp 28 26.3), (Temp 33 24.8)] == 33 -- my test
