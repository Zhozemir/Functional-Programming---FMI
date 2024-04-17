import Data.List -- for 2nd Task



-- 1st Task
numStepCombinations :: Integer -> Integer
numStepCombinations n
 | n < 0 = error "n has to be non-negative"
 | otherwise = numStepCombinationsHelper (n+1)
  where 
    numStepCombinationsHelper :: Integer -> Integer
    numStepCombinationsHelper n = helper 0 1 n
     where
      helper :: Integer -> Integer -> Integer -> Integer
      helper n0 _ 0 = n0
      helper _ n1 1 = n1
      helper n0 n1 leftover = helper n1 (n0 + n1) (leftover - 1)



-- 2nd Task
persistence :: Int -> Int
persistence n
 | n < 10 = 0
 | otherwise = 1 + persistence (persistenceOfNumbers n)
  where
   persistenceOfNumbers :: Int -> Int
   persistenceOfNumbers n = helper n 1
    where
     helper 0 acc = acc
     helper x acc
      | digit == 0 = helper (div x 10) acc
      | otherwise = helper (div x 10) $ acc * digit
       where
        digit = mod x 10

maxPersistenceMinSum :: Int -> Int -> Int
maxPersistenceMinSum start end = minimumBySum [x | x <- [start..end], persistence x == maxPersistence]
     where
      maxPersistence = maximum [persistence x | x <- [start..end]]
      minimumBySum (x:xs) = currHelper x xs
       where
        currHelper acc [] = acc
        currHelper acc (y:ys)
          | digitSum y < digitSum acc = currHelper y ys
          | otherwise = currHelper acc ys
       
        digitSum :: Int -> Int
        digitSum n
         | n < 10 = n
         | otherwise = mod n 10 + digitSum (div n 10)



main :: IO ()
main = do
 print $ numStepCombinations 2 == 2
 print $ numStepCombinations 3 == 3
 print $ numStepCombinations 100 == 573147844013817084101
 print $ maxPersistenceMinSum 273 392
 print $ maxPersistenceMinSum 1000 2000
 print $ maxPersistenceMinSum 55 105
 print $ maxPersistenceMinSum 195 756
 print $ maxPersistenceMinSum 2 85