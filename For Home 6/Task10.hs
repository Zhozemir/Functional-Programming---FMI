checkNumber :: Int -> (Int, Int)
checkNumber n = (sum $ digitsAtIndex evenIndices, sum $ digitsAtIndex oddIndices)
  where
    digits = map (\x -> read [x] :: Int) (show n)
    evenIndices = filter even [0..length digits - 1]
    oddIndices = filter odd [0..length digits - 1]
    digitsAtIndex indices = map (\i -> digits !! i) indices
    
main :: IO ()
main = do
 print $ checkNumber 2728 == (4,15)
 print $ checkNumber 31415 == (12,2)
 print $ checkNumber 121 == (2,2)
 print $ checkNumber 345 == (8,4)  -- my test
