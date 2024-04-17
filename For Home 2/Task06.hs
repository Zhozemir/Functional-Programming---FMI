sumDigits :: Int -> Int
sumDigits n
 | n < 0 = error "n must be non-negative"
 | otherwise = helper n 0
  where
   helper 0 result = result
   helper n result = helper (div n 10) $ result + (mod n 10)

isInteresting :: Int -> Bool
isInteresting x = mod x (sumDigits x) == 0

main :: IO ()
main = do
 print $ isInteresting 410 == True
 print $ isInteresting 212 == False
 print $ isInteresting 567 == False
 print $ isInteresting 70 == True
 print $ isInteresting 5 == True
 print $ isInteresting 4 == True
 print $ isInteresting 12 == True -- my test 
