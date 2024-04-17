switchSum :: (Num a) => (a -> a) -> (a -> a) -> Int -> a -> a
switchSum _ _ 0 _ = 0
switchSum f g n x = f x + switchSum g f (n-1) (f x)

main :: IO()
main = do
 print $ (switchSum (\x -> x + 1) (\x -> x * 2) 1) 2 == 3
 print $ (switchSum (\x -> x + 1) (\x -> x * 2) 2) 2 == 9
 print $ (switchSum (\x -> x + 1) (\x -> x * 2) 3) 2 == 16
 print $ (switchSum (\x -> x + 1) (\x -> x * 2) 4) 2 == 30
 print $ (switchSum (\x -> x + 1) (\x -> x * 2) 5) 2 == 45 -- my test