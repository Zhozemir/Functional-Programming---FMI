findSum :: Int -> Int -> Int -> Int
findSum a b n = helper a b (n - 1) 0 + helper a b (n - 2) 0 + helper a b (n - 3) 0
 where 
  helper :: Int -> Int -> Int -> Int -> Int
  helper a b 0 result = a + b + result
  helper a b pow result = helper a b (pow - 1) $ result + b * 2 ^ pow
    
main :: IO()
main = do
 print $ findSum 0 2 10  == 3578 
 print $ findSum 5 3 5  == 174
 print $ findSum 5 4 11 == 14339 -- my test

    
