myPoly :: [Double] -> (Double -> Int -> Double)
myPoly xs = (\x y -> product $ map (x -) (take y xs))

main :: IO ()
main = do

 print $ (myPoly [2.7, 3.0 ..] ) 2.2 3 == -0.4399999999999998
 print $ (myPoly [2.0, 4.0 ..] ) 3.0 5 == 105.0 -- my test
