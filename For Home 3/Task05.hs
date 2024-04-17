p :: Int -> Int
p n = helper n 1
 where
  helper :: Int -> Int -> Int
  helper 1 result = result
  helper n result = helper (n - 1) (result + 3 * (n - 1) + 1)

main :: IO()
main = do

 print $ p 1 == 1
 print $ p 2 == 5
 print $ p 3 == 12
 print $ p 4 == 22
 print $ p 5 == 35
 print $ p 6 == 51
 print $ p 7 == 70 -- my test
