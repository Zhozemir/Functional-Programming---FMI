isPalindrome :: Int -> Bool
isPalindrome n = (read $ reverse $ show n) == n

getPalindromes :: Int -> Int
getPalindromes n = head filtered + last filtered
 where
    filtered = filter (\x -> isPalindrome x && mod n x == 0) [2..n]

main :: IO()
main = do

 print $ getPalindromes 132465 == 8
 print $ getPalindromes 654546 == 8
 print $ getPalindromes 100001 == 100012
 print $ getPalindromes 21612 == 21614
 print $ getPalindromes 26362 == 26364
 print $ getPalindromes 567 == 12 -- my test