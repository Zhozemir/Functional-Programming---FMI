everyOther :: Int -> Int
everyOther n = helper n 0 1
  where
   helper :: Int -> Int -> Int -> Int
   helper 0 result counter = result
   helper number result counter
    |odd counter = helper (div number 10) result $ counter + 1
    |otherwise = helper (div number 10) (result * 10 + mod number 10) $ counter + 1

main :: IO()
main = do
 print $ everyOther 12 == 1
 print $ everyOther 852369 == 628
 print $ everyOther 1714 == 11
 print $ everyOther 12345 == 42
 print $ everyOther 891 == 9
 print $ everyOther 123 == 2
 print $ everyOther 2121 == 22
 print $ everyOther 4736778 == 767
 print $ everyOther 448575 == 784
 print $ everyOther 4214 == 14
 print $ everyOther 325167 == 653 -- my test
