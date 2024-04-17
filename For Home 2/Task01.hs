countDigitsIter :: Int -> Int
countDigitsIter n
 | n < 0 = error "n has to be non-negative"
 | otherwise = helper n 0
  where
   helper 0 0 = 1 
   helper 0 result = result
   helper n result = helper (div n 10) $ result + 1
   
countDigitsRec :: Int -> Int
countDigitsRec n
 | n < 0 = error "n has to be non-negative"
 | n == 0 = 0 
 | otherwise = 1 + countDigitsRec (div n 10) 

main :: IO()
main = do

print $ countDigitsIter 12345 == 5
print $ countDigitsIter 123 == 3
print $ countDigitsIter 4567 == 4 -- my case 

print $ countDigitsRec 12345 == 5
print $ countDigitsRec 123 == 3
print $ countDigitsIter 4567 == 4 -- my case