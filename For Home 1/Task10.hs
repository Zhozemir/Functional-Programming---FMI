snail :: Int -> Int -> Int -> Int
snail wantedHeight daySpeed nightSpeed = crawl 0 0
 where
  crawl currentHeight days
   | currentHeight >= wantedHeight = days
   | currentHeight + daySpeed >= wantedHeight = days + 1
   | otherwise = crawl (currentHeight + daySpeed - nightSpeed) $ days + 1

main :: IO ()
main = do
 print $ snail 3 2 1 == 2
 print $ snail 10 3 1 == 5
 print $ snail 10 3 2 == 8
 print $ snail 100 20 5 == 7
 print $ snail 5 10 3 == 1
 print $ snail 11 7 4 == 3 -- my test