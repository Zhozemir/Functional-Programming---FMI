rev :: Int -> Int
rev n = helper n 0
 where
  helper :: Int -> Int -> Int
  helper 0 result = result
  helper leftover result = helper (div leftover 10) $ result * 10 + mod leftover 10

isPalindrome :: Int -> Bool
isPalindrome n
 | n < 0 = error "n has to be non-negative"
 | otherwise = n == rev n

countPalindromes :: Int -> Int -> Int
countPalindromes a b = helper (min a b + 1) (max a b - 1) 0
 where
  helper :: Int -> Int -> Int -> Int
  helper x y count
   |x > y = count
   |isPalindrome x = helper (x + 1) y $ count + 1
   |otherwise = helper (x + 1) y count

main:: IO ()   
main = do
 print $ countPalindromes 5 13 == 5 
 print $ countPalindromes 13 5 == 5 
 print $ countPalindromes 8 13 == 2 -- my test