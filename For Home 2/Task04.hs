countOccurrences :: Int -> Int -> Int
countOccurrences n m 
 | n < 0 = error "n has to be positive number"
 | n == 0 && m == 0 = 1
 | otherwise = helper n 0
  where
   helper 0 result = result
   helper n result
    | mod n 10 == m = helper (div n 10) $ result + 1
    | otherwise = helper (div n 10) result

main :: IO()
main = do
 print $ countOccurrences 121 1 == 2
 print $ countOccurrences 222 1 == 0
 print $ countOccurrences 100 0 == 2
 print $ countOccurrences 0 0 == 1
 print $ countOccurrences 34454 4 == 3 -- my test
