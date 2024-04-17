import Data.List

isAscending :: Int -> Bool
isAscending n = sortedDigits == digits
 where
  digits = map (\x -> read [x] :: Int) (show n)
  sortedDigits = sort digits

main :: IO ()
main = do
 print $ isAscending 0 == True
 print $ isAscending 10 == False
 print $ isAscending 123 == True
 print $ isAscending 1233 == True
 print $ isAscending 12332 == False
 print $ isAscending 12345 == True -- my test
