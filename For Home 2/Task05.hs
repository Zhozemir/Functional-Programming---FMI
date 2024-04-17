sumDivs :: Int -> Int
sumDivs n = helper 0 1
 where
  helper :: Int -> Int -> Int
  helper result divisor
   | divisor >= n = result
   | mod n divisor == 0 = helper (result + divisor) $ divisor + 1
   | otherwise = helper result $ divisor + 1 

areAmicable :: Int -> Int -> Bool
areAmicable x y 
 | sumDivs x == y || sumDivs y == x = True
 | otherwise = False

main :: IO()
main = do
 print $ areAmicable 200 300 == False
 print $ areAmicable 220 284 == True
 print $ areAmicable 284 220 == True
 print $ areAmicable 1184 1210 == True
 print $ areAmicable 2620 2924 == True
 print $ areAmicable 6232 6368 == True
 print $ areAmicable 9 4 == True -- my test