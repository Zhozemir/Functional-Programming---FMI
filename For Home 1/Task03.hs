isEvenIf :: Int -> String
isEvenIf n = if mod n 2 == 0 then "Yes" else "No"

isEvenGuard :: Int -> String
isEvenGuard n
 | mod n 2 == 0 = "Yes"
 | otherwise = "No"
 
main :: IO()
main = do
   
     print $ isEvenIf 2 == "Yes"
     print $ isEvenIf 15452 == "Yes"
     print $ isEvenIf 321 == "No"
     print $ isEvenIf 624 == "Yes" -- my test

     print $ isEvenGuard 2 == "Yes"
     print $ isEvenGuard 15452 == "Yes"
     print $ isEvenGuard 321 == "No"
     print $ isEvenGuard 543 == "No" -- my test

