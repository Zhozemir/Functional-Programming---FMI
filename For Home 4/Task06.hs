import Data.List 
import Data.Char 

sumSpecialPrimes :: Int -> Int -> Int
sumSpecialPrimes n d = sum (take n (filter (\x -> any (\c -> digitToInt c == d) (show x) && isPrime x) [2..]))
 where
  isPrime :: Int -> Bool
  isPrime n = n > 1 && helper 2
   where
    helper divisor
     | divisor >= n = True
     | mod n divisor == 0 = False
     | otherwise = helper $ divisor + 1

main :: IO ()
main = do
 print $ sumSpecialPrimes 5 2 == 392 
 print $ sumSpecialPrimes 5 3 == 107
 print $ sumSpecialPrimes 10 3 == 462
 print $ sumSpecialPrimes 4 1 == 60
