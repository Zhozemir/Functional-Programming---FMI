growingPlant :: Int -> Int -> Int -> Int
growingPlant upSpeed downSpeed wantedHeight = grow 0 0
 where
  grow currentHeight days
   | currentHeight >= wantedHeight = days
   | currentHeight + upSpeed >= wantedHeight = days + 1
   | otherwise = grow (currentHeight + upSpeed - downSpeed) $ days + 1

main :: IO ()
main = do
 print $ growingPlant 5 2 5 == 1
 print $ growingPlant 5 2 6 == 2
 print $ growingPlant 10 9 4 == 1
 print $ growingPlant 100 10 910 == 10
 print $ growingPlant 4 1 6 == 2 -- my test
