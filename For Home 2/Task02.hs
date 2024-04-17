sumDigitsIter :: Int -> Int
sumDigitsIter n
  | n < 0     = error "n must be non-negative"
  | otherwise = helper n 0
  where
    helper 0 result = result
    helper n result = helper (div n 10) $ result + (mod n 10) 

main :: IO()
main = do
  print $ sumDigitsIter 12345 == 15  
  print $ sumDigitsIter 123 == 6     
  print $ sumDigitsIter 5678 == 26 -- my case 
