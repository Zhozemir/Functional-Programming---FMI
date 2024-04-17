sumDigits :: Int -> Int
sumDigits 0 = 0
sumDigits n = mod n 10 + (sumDigits $ div n 10)

sumDivisibleNumbers :: Int -> Int -> Int -> Int
sumDivisibleNumbers start finish k = helper (min start finish) (max start finish) k
 where
    helper :: Int -> Int -> Int -> Int
    helper hStart hFinish divisor
     | hStart > hFinish = 0
     | mod (sumDigits hStart) divisor == 0 = hStart + helper (hStart + 1) hFinish divisor
     | otherwise = helper (hStart + 1) hFinish divisor

main :: IO()
main = do
 print $ sumDivisibleNumbers 50 10 5 == 290
 print $ sumDivisibleNumbers 0 10 5 == 5
 print $ sumDivisibleNumbers 0 100 5 == 990
 print $ sumDivisibleNumbers 100 0 5 == 990
 print $ sumDivisibleNumbers 50 0 5 == 295
    
