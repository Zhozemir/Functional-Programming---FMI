isPrime :: Int -> Bool
isPrime n = n > 1 && helper 2
 where
  helper divisor
   | divisor >= n = True
   | mod n divisor == 0 = False
   | otherwise = helper $ divisor + 1

sumPrimeDivs :: Int -> Int
sumPrimeDivs n = sumPrimeDivsHelper n 2 0
 where
  sumPrimeDivsHelper :: Int -> Int -> Int -> Int
  sumPrimeDivsHelper num divisor acc
   | divisor > num = acc
   | mod num divisor == 0 && isPrime divisor = sumPrimeDivsHelper num (divisor + 1) $ acc + divisor
   | otherwise = sumPrimeDivsHelper num (divisor + 1) acc

main :: IO ()
main = do
 print $ sumPrimeDivs 0 == 0
 print $ sumPrimeDivs 6 == 5 
 print $ sumPrimeDivs 18 == 5
 print $ sumPrimeDivs 19 == 19
 print $ sumPrimeDivs 45136 == 53
 print $ sumPrimeDivs 12 == 5 -- my test
