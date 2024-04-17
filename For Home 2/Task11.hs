isPrime :: Int -> Bool
isPrime n = n > 1 && helper 2
 where
  helper divisor
   |divisor >= n = True
   | mod n divisor == 0 = False
   | otherwise = helper $ divisor + 1

truncatablePrime :: Int -> Bool
truncatablePrime n
 | n <= 1 = False
 | n < 10 = isPrime n
 | otherwise = isPrime n && truncatablePrime (div n 10)

main :: IO ()
main = do
 print $ truncatablePrime 3797 == True
 print $ truncatablePrime 47 == False 
 print $ truncatablePrime 0 == False
 print $ truncatablePrime 1 == False
 print $ truncatablePrime 2 == True
 print $ truncatablePrime 37397 == True
 print $ truncatablePrime 1399 == False 
 print $ truncatablePrime 1733 == False 
 print $ truncatablePrime 1913 == False 
 print $ truncatablePrime 1931 == False 
 print $ truncatablePrime 1933 == False 
 print $ truncatablePrime 1973 == False 
 print $ truncatablePrime 19333 == False 
 print $ truncatablePrime 19739 == False 
 print $ truncatablePrime 73 == True -- my test
 