reverseNumber :: Int -> Int
reverseNumber n = helper n 0
 where
  helper 0 reversed = reversed
  helper remaining reversed = helper (div remaining 10) $ reversed * 10 + mod remaining 10

removeD :: Int -> Int -> Int
removeD d n = reverseNumber $ helper n 0
 where
  helper 0 result = result
  helper n result
   | mod n 10 == d = helper (div n 10) result
   | otherwise = helper (div n 10) $ result * 10 + mod n 10

main :: IO ()
main = do
 print $ removeD 1 656 == 656
 print $ removeD 5 656 == 66
 print $ removeD 6 656 == 5
 print $ removeD 0 606 == 66
 print $ removeD 0 600 == 6
 print $ removeD 6 600 == 0
 print $ removeD 2 1234 == 134
 print $ removeD 5 476589 == 47689 -- my test
