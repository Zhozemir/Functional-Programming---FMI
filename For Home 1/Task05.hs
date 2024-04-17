sqAvg :: Int -> Int -> Double
sqAvg n k = (fromIntegral n^2 + fromIntegral k^2) / 2

main :: IO()
main = do
 
   print $ sqAvg 5 0 == 12.5
   print $ sqAvg 10 13 == 134.5
   print $ sqAvg 3 2 == 6.5 -- my test