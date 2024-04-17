pairCompose :: [Int -> Int] -> (Int -> Int)
pairCompose [] = id
pairCompose[f] = f 
pairCompose (f1:f2:xs) = (\x -> f1 $ f2 x + pairCompose xs x)

main :: IO()
main = do
 print $ (pairCompose [(+1), (+2)]) 1 == 5
 print $ (pairCompose [(+1), (+2), (+3)]) 1 == 8
 print $ (pairCompose [(+3), (+4), (+5)]) 1 == 14 -- my test